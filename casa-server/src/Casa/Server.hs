{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Casa content-addressable storage archive server.

module Casa.Server
  ( App(..)
  , resourcesApp
  , Widget
  , hashesFromStream
  , withDBPool
  , migrateAll
  , Content(..)
  , ContentId
  ) where

import           Casa.Types
import           Control.Applicative
import           Control.Monad.Logger
import qualified Crypto.Hash as Crypto
import qualified Data.Attoparsec.Binary as Atto.B
import qualified Data.Attoparsec.ByteString as Atto.B
import qualified Data.ByteArray as Mem
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Builder as S
import qualified Data.ByteString.Builder as SB
import qualified Data.ByteString.Char8 as S8
import           Data.Conduit
import           Data.Conduit.Attoparsec
import qualified Data.Conduit.List as CL
import           Data.Foldable
import           Data.Functor
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import           Data.Maybe
import           Data.Pool
import qualified Data.Text as T
import           Data.Word
import qualified Database.Esqueleto as E
import           Database.Persist.Postgresql
import           System.Environment
import           Yesod hiding (Content)

--------------------------------------------------------------------------------
-- Constants

maximumContentLen :: Word64
maximumContentLen = (1024 * 50)

--------------------------------------------------------------------------------
-- Types

-- | Server app.
data App =
  App
    { appLogging :: !Bool
    , appPool :: !(Pool SqlBackend)
    }

instance Yesod App where
  maximumContentLength _ _ = Just maximumContentLen
  makeSessionBackend _ = return Nothing
  shouldLogIO app src level =
    if appLogging app
      then defaultShouldLogIO src level
      else pure False

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        App {appPool} <- getYesod
        runSqlPool action appPool

instance YesodPersistRunner App where
  getDBRunner = defaultGetDBRunner appPool

--------------------------------------------------------------------------------
-- Model

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Content
  key BlobKey
  blob ByteString
  Unique BlobUniqueKey key
|]

--------------------------------------------------------------------------------
-- Routes

mkYesod "App" [parseRoutesNoCheck|
  /v1/pull PullR POST
  /v1/push PushR POST
  /#BlobKey KeyR GET
|]

--------------------------------------------------------------------------------
-- Handlers

-- | Get a single blob in a web interface.
getKeyR :: BlobKey -> Handler TypedContent
getKeyR blobKey = do
  contents <-
    runDB
      (E.select
         (E.from
            (\content -> do
               E.where_ (content E.^. ContentKey E.==. E.val blobKey)
               return (content E.^. ContentBlob))))
  case listToMaybe contents of
    Nothing -> notFound
    Just (E.Value bytes) ->
      pure
        (TypedContent
           "application/octet-stream"
           (ContentBuilder (S.byteString bytes) (Just (S.length bytes))))

-- | Push a batch of blobs.
postPushR :: Handler TypedContent
postPushR =
  respondSourceDB
    "application/octet-stream"
    (blobsFromBody .|
     awaitForever
       (\result ->
          case result of
            Left err ->
              invalidArgs [T.pack ("Invalid (len,blob) pair: " ++ show err)]
            Right (blobKey, blob) ->
              lift
                (void
                   (insertUnique
                      (Content {contentKey = blobKey, contentBlob = blob})))))

-- | Pull a batch of blobs.
postPullR :: Handler TypedContent
postPullR = do
  keyLenPairs <- keyLenPairsFromBody
  let keys = fmap fst keyLenPairs
      source =
        E.selectSource
          (E.from
             (\content -> do
                E.where_
                  (content E.^. ContentKey `E.in_` E.valList (toList keys))
                return (content E.^. ContentKey, content E.^. ContentBlob)))
  -- We return a stream of key+blob pairs in binary format. The client
  -- knows how long the hash should be and how long the blob should be
  -- based on the hash.
  respondSourceDB
    "application/octet-stream"
    (source .|
     CL.concatMap
       (\(E.Value blobKey,E.Value blob) ->
          [ Chunk (blobKeyToBuilder blobKey <> SB.byteString blob)
          , Flush -- Do we want to flush after every blob?
          ]))

--------------------------------------------------------------------------------
-- Input reader

-- | Read the list of content blobs from the body.
blobsFromBody ::
     (MonadHandler m)
  => ConduitT i (Either ParseError (BlobKey, ByteString)) m ()
blobsFromBody = do
  rawRequestBody .| blobsFromStream

-- | Read blobs from a stream.
blobsFromStream ::
     Monad m => ConduitT ByteString (Either ParseError (BlobKey, ByteString)) m ()
blobsFromStream = conduitParserEither lenBlobParser .| CL.map (fmap snd)
  where
    lenBlobParser = do
      len <- fmap fromIntegral Atto.B.anyWord64be
      bytes <- Atto.B.take len
      pure (BlobKey (sha256Hash bytes), bytes)

-- | Read the list of hashes from the body.
keyLenPairsFromBody ::
     (MonadHandler m)
  => m (NonEmpty (BlobKey, Int))
keyLenPairsFromBody = do
  result <- runConduit (rawRequestBody .| hashesFromStream)
  case result of
    Left err ->
      invalidArgs
        ["Invalid blob keys, parse error: " <> T.pack (errorMessage err)]
    Right keys -> do
      case NE.nonEmpty keys of
        Nothing -> invalidArgs ["No keys provided."]
        Just nonEmpty -> pure nonEmpty

-- | Read hashes from a stream.
hashesFromStream ::
     Monad m => ConduitT ByteString o m (Either ParseError [(BlobKey, Int)])
hashesFromStream =
  sinkParserEither (some keyValueParser)
  where
    keyValueParser =
      (,) <$> blobKeyBinaryParser <*> fmap fromIntegral Atto.B.anyWord64be

--------------------------------------------------------------------------------
-- DB connection

withDBPool ::
     (IsPersistBackend b, BaseBackend b ~ SqlBackend)
  => (Pool b -> LoggingT IO a)
  -> IO a
withDBPool cont = do
  dbstr <- getEnv "DBCONN"
  runStdoutLoggingT
    (withPostgresqlPool
       (S8.pack dbstr)
       10
       cont)

--------------------------------------------------------------------------------
-- Hashing

sha256Hash :: ByteString -> ByteString
sha256Hash = Mem.convert . Crypto.hashWith Crypto.SHA256
