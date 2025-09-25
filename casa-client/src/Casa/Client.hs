{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveLift        #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Types and functions for a client for Casa (Content-addressable Storage
-- Archive).

module Casa.Client
  ( blobsSource
  , SourceConfig (..)
  , blobsSink
  , CasaRepoPrefix
  , parseCasaRepoPrefix
  , thParserCasaRepo
  , PushException (..)
  , PullException (..)
  ) where

import           Casa.Types
                   ( BlobKey (..), blobKeyBinaryParser, blobKeyToBuilder )
import           Control.Monad ( (>=>), unless )
import           Control.Monad.Catch ( Exception, MonadThrow, throwM )
import           Control.Monad.IO.Class ( MonadIO )
import           Control.Monad.IO.Unlift
                   ( MonadUnliftIO, UnliftIO (..), askUnliftIO )
import           Control.Monad.Trans.Resource ( MonadResource )
import qualified Crypto.Hash as Crypto
import           Data.Aeson ( FromJSON (..) )
import qualified Data.Attoparsec.ByteString as Atto
import qualified Data.ByteArray as Mem
import           Data.ByteString ( ByteString )
import qualified Data.ByteString as S
import qualified Data.ByteString.Builder as SB
import           Data.Conduit ( ConduitT, (.|), await, transPipe, yield )
import           Data.Conduit.Attoparsec ( ParseError, conduitParserEither )
import           Data.Conduit.ByteString.Builder ( builderToByteString )
import qualified Data.Conduit.List as CL
import           Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as HM
#if !MIN_VERSION_base(4,20,0)
import           Data.Foldable ( foldl' )
#endif
import           Language.Haskell.TH ( Exp, Q )
import           Language.Haskell.TH.Lift ( Lift (..) )
import           Network.HTTP.Client.Conduit ( requestBodySourceChunked )
import           Network.HTTP.Simple
                   ( Request, getResponseBody, getResponseStatus
                   , httpNoBody, httpSource, parseRequest, setRequestBody
                   , setRequestBodyLBS, setRequestMethod
                   )
import           Network.HTTP.Types ( Status (..) )
import           Network.URI ( parseURI )

-- | An exception from blob consuming/sending.
data PullException
  = AttoParseError ParseError
  | BadHttpStatus Status
  | TooManyReturnedKeys Int
  deriving Show

instance Exception PullException

-- | An exception from blob consuming/sending.
newtype PushException
  = PushBadHttpStatus Status
  deriving Show

instance Exception PushException

-- | The URL prefix for a Casa repository, commonly @https://casa.stackage.org@.
-- Parsers will strip out a trailing slash.
newtype CasaRepoPrefix =
  CasaRepoPrefix String
  deriving (Show, Lift)

instance FromJSON CasaRepoPrefix where
  parseJSON = parseJSON >=> (either fail pure . parseCasaRepoPrefix)

-- | TH compile-time parser.
thParserCasaRepo :: String -> Q Exp
thParserCasaRepo = either error lift . parseCasaRepoPrefix

-- | Parse and normalize a Casa repo prefix.
parseCasaRepoPrefix :: String -> Either String CasaRepoPrefix
parseCasaRepoPrefix s =
  case parseURI s of
    Nothing ->
      Left
        "Invalid URI for repository. Should be a valid URI e.g. https://casa.stackage.org"
    Just {} -> pure (CasaRepoPrefix (stripTrailing s))
  where
    stripTrailing = reverse . dropWhile (== '/') . reverse

-- | Used to build request paths.
casaServerVersion :: String
casaServerVersion = "v1"

-- | Build the URL from a repo prefix.
casaRepoPushUrl :: CasaRepoPrefix -> String
casaRepoPushUrl (CasaRepoPrefix uri) =
  uri <> "/" <> casaServerVersion <> "/push"

-- | Build the URL from a repo prefix.
casaRepoPullUrl :: CasaRepoPrefix -> String
casaRepoPullUrl (CasaRepoPrefix uri) =
  uri <> "/" <> casaServerVersion <> "/pull"

-- | A sink to push blobs to the server. Throws 'PushException'.
blobsSink ::
     (MonadIO m, MonadThrow m, MonadUnliftIO m)
  => CasaRepoPrefix
  -> ConduitT () ByteString m ()
  -> m ()
blobsSink casaRepoUrl blobs = do
  runInIO <- askUnliftIO
  request <- makeRequest runInIO
  response <- httpNoBody request
  case getResponseStatus response of
    Status 200 _ -> pure ()
    status -> throwM (PushBadHttpStatus status)
  where
    makeRequest (UnliftIO runInIO) =
      fmap
        ( setRequestBody
            ( requestBodySourceChunked
                (  transPipe runInIO blobs
                .| CL.map
                     ( \v ->
                            SB.word64BE (fromIntegral (S.length v))
                         <> SB.byteString v
                     )
                .| builderToByteString
                )
            )
        . setRequestMethod "POST"
        )
        (parseRequest (casaRepoPushUrl casaRepoUrl))

-- | Configuration for sourcing blobs from the server.
data SourceConfig =
  SourceConfig
    { sourceConfigUrl :: !CasaRepoPrefix
      -- ^ URL to pull from.
    , sourceConfigBlobs :: !(HashMap BlobKey Int)
      -- ^ The blobs to pull.
    , sourceConfigMaxBlobsPerRequest :: !Int
      -- ^ Maximum number of blobs per request; we split requests into chunks of
      -- this number.
    }

-- | Make a source of blobs from a URL. Throws 'PullException'.
blobsSource ::
     (MonadThrow m, MonadResource m, MonadIO m)
  => SourceConfig
  -> ConduitT i (BlobKey, ByteString) m ()
blobsSource sourceConfig = do
  skeletonRequest <- makeSkeletonRequest
  source skeletonRequest scBlobsList .| conduit .| consumer scBlobsSize
 where
  makeSkeletonRequest =
    fmap
      (setRequestMethod "POST")
      (parseRequest (casaRepoPullUrl (sourceConfigUrl sourceConfig)))
  scBlobs = sourceConfigBlobs sourceConfig
  scBlobsList = HM.toList scBlobs
  scBlobsSize = HM.size scBlobs
  source skeletonRequest blobs =
    unless (null blobs) $ do
      httpSource filledRequest $ \response ->
        case getResponseStatus response of
          Status 200 _ -> getResponseBody response
          status -> throwM (BadHttpStatus status)
      source skeletonRequest remainingBlobs
   where
    (filledRequest, remainingBlobs) =
      setRequestBlobs sourceConfig blobs skeletonRequest
  conduit = conduitParserEither (blobKeyValueParser scBlobs)
  consumer remaining = await >>= \case
    Nothing -> pure ()
    Just (Left x) -> throwM (AttoParseError x)
    Just (Right (_position, keyValue)) ->
      if remaining == 0
        then throwM (TooManyReturnedKeys scBlobsSize)
        else do
          yield keyValue
          consumer (remaining - 1)

-- | Fill the body of the request with max blobs per request.
setRequestBlobs ::
  SourceConfig -> [(BlobKey, Int)] -> Request -> (Request, [(BlobKey, Int)])
setRequestBlobs sourceConfig blobs skeletonRequest = (request, remaining)
  where
    request =
      setRequestBodyLBS
        ( SB.toLazyByteString
            ( foldl'
                (\a (k, v) ->
                   a <> (blobKeyToBuilder k <> SB.word64BE (fromIntegral v)))
                mempty
                thisBatch
            )
        )
        skeletonRequest
    (thisBatch, remaining) =
      splitAt (sourceConfigMaxBlobsPerRequest sourceConfig) blobs

-- | Parser for a key/value.
blobKeyValueParser :: HashMap BlobKey Int -> Atto.Parser (BlobKey, ByteString)
blobKeyValueParser lengths = do
  blobKey <- blobKeyBinaryParser
  case HM.lookup blobKey lengths of
    Nothing -> fail $ "Invalid key: " <> show blobKey
    Just len -> do
      blob <- Atto.take len
      if BlobKey (sha256Hash blob) == blobKey
        then pure (blobKey, blob)
        else fail $ "Content does not match SHA256 hash: " <> show blobKey

-- | Hash some raw bytes.
sha256Hash :: ByteString -> ByteString
sha256Hash = Mem.convert . Crypto.hashWith Crypto.SHA256
