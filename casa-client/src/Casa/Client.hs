{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

-- |

module Casa.Client
  ( blobsSource
  , blobsSink
  ) where

import           Casa.Types
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.IO.Unlift
import           Control.Monad.Trans.Resource
import qualified Crypto.Hash as Crypto
import qualified Data.Attoparsec.ByteString as Atto
import qualified Data.ByteArray as Mem
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Builder as SB
import           Data.Conduit
import           Data.Conduit.Attoparsec
import           Data.Conduit.ByteString.Builder
import qualified Data.Conduit.List as CL
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Typeable
import           Network.HTTP.Client.Conduit (requestBodySourceChunked)
import           Network.HTTP.Simple
import           Network.HTTP.Types

-- | An exception from blob consuming/sending.
data PullException
  = AttoParseError ParseError
  | BadHttpStatus Status
  | TooManyReturnedKeys Int
  deriving (Show, Typeable)
instance Exception PullException

-- | An exception from blob consuming/sending.
data PushException
  = PushBadHttpStatus Status
  deriving (Show, Typeable)
instance Exception PushException

-- | A sink to push blobs to the server. Throws 'PushException'.
blobsSink ::
     (MonadIO m, MonadThrow m, MonadUnliftIO m)
  => String
  -> ConduitT () ByteString m ()
  -> m ()
blobsSink url blobs = do
  runInIO <- askUnliftIO
  request <- makeRequest runInIO
  response <- httpNoBody request
  case getResponseStatus response of
    Status 200 _ -> pure ()
    status -> throwM (PushBadHttpStatus status)
  where
    makeRequest (UnliftIO runInIO) =
      fmap
        (setRequestBody
           (requestBodySourceChunked
              (transPipe runInIO blobs .|
               CL.map
                 (\v ->
                    SB.word64BE (fromIntegral (S.length v)) <> SB.byteString v) .|
               builderToByteString)) .
         setRequestMethod "POST")
        (parseRequest url)

-- | Make a source of blobs from a URL. Throws 'PullException'.
blobsSource ::
     (MonadThrow m, MonadResource m, MonadIO m)
  => String
  -> HashMap BlobKey Int
  -> ConduitT i (BlobKey, ByteString) m ()
blobsSource url keyLengths = do
  request <- makeRequest
  source request .| conduit .| consumer (HM.size keyLengths)
  where
    makeRequest =
      fmap
        (setRequestBodyLBS
           (SB.toLazyByteString
              (HM.foldlWithKey'
                 (\a k v ->
                    blobKeyToBuilder k <> SB.word64BE (fromIntegral v) <> a)
                 mempty
                 keyLengths)) .
         setRequestMethod "POST")
        (parseRequest url)
    source request =
      httpSource
        request
        (\response ->
           case getResponseStatus response of
             Status 200 _ -> getResponseBody response
             status -> throwM (BadHttpStatus status))
    conduit = conduitParserEither (blobKeyValueParser keyLengths)
    consumer remaining = do
      mkeyValue <- await
      case mkeyValue of
        Nothing -> pure ()
        Just (Left x) -> throwM (AttoParseError x)
        Just (Right (_position, keyValue)) ->
          if remaining == 0
            then throwM (TooManyReturnedKeys (HM.size keyLengths))
            else do
              yield keyValue
              consumer (remaining - 1)

-- | Parser for a key/value.
blobKeyValueParser :: HashMap BlobKey Int -> Atto.Parser (BlobKey, ByteString)
blobKeyValueParser lengths = do
  blobKey <- blobKeyBinaryParser
  case HM.lookup blobKey lengths of
    Nothing -> fail ("Invalid key: " <> show blobKey)
    Just len -> do
      blob <- (Atto.take len)
      if BlobKey (sha256Hash blob) == blobKey
        then pure (blobKey, blob)
        else fail ("Content does not match SHA256 hash: " ++ show blobKey)

-- | Hash some raw bytes.
sha256Hash :: ByteString -> ByteString
sha256Hash = Mem.convert . Crypto.hashWith Crypto.SHA256
