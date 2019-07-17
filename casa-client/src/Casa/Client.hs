{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

-- |

module Casa.Client
  ( blobsSource
  ) where

import           Casa.Types
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import qualified Data.Attoparsec.ByteString as Atto
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as SB
import           Data.Conduit
import           Data.Conduit.Attoparsec
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text)
import           Data.Typeable
import           Network.HTTP.Simple
import           Network.HTTP.Types

-- | An exception from blob consuming.
data BlobsException
  = AttoParseError ParseError
  | BadHttpStatus Status
  | TooManyReturnedKeys Int
  deriving (Show, Typeable)
instance Exception BlobsException

-- | Make a source of blobs from a URL. Throws 'BlobsException', so catch that.
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
    Just len -> fmap (blobKey, ) (Atto.take len)

-- | Make a partial key.
partialKey :: Text -> BlobKey
partialKey = either error id . blobKeyHexParser
{-# DEPRECATED partialKey "This is just for debugging." #-}
