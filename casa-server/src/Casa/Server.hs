{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
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
  , AccessLog(..)
  , AccessLogId
  ) where

import           Casa.Backend
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
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import           Data.Word
import qualified Database.Esqueleto as E
import           Prelude hiding (log)
import           System.Environment
import           Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
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
    , appAuthorized :: !AuthResult
    }

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
  created UTCTime default=CURRENT_TIMESTAMP
  Unique BlobUniqueKey key
AccessLog
  key BlobKey
  count Int
  lastAccess UTCTime
  Unique AccessUnqiueKey key
|]

--------------------------------------------------------------------------------
-- Routes

mkYesod "App" [parseRoutesNoCheck|
  / HomeR GET
  /v1/pull V1PullR POST
  /v1/push V1PushR POST
  /v1/metadata/#BlobKey V1MetadataR GET
  /#BlobKey KeyR GET
  /stats StatsR GET
  /liveness LiveR GET
|]

instance Yesod App where
  isAuthorized route _ignored = do
    app <- getYesod
    case route of
      V1PushR -> pure (appAuthorized app)
      V1PullR -> pure Authorized
      KeyR {} -> pure Authorized
      V1MetadataR {} -> pure Authorized
      HomeR -> pure Authorized
      StatsR -> pure Authorized
      LiveR -> pure Authorized
  maximumContentLength _ mroute =
    case mroute of
      Nothing -> Just maximumContentLen
      Just V1PullR -> Just maximumContentLen
      Just KeyR {} -> Just maximumContentLen
      Just V1PushR {} -> Nothing
      Just V1MetadataR {} -> Just maximumContentLen
      Just HomeR {} -> Just maximumContentLen
      Just StatsR {} -> Just maximumContentLen
      Just LiveR {} -> Just maximumContentLen
  makeSessionBackend _ = return Nothing
  shouldLogIO app src level =
    if appLogging app
      then defaultShouldLogIO src level
      else pure False

--------------------------------------------------------------------------------
-- Handlers

getLiveR :: Handler Text
getLiveR = do
  now <- liftIO getCurrentTime
  status <- runDB (E.select (pure (E.val "Database connectivity is OK!")))
  later <- liftIO getCurrentTime
  pure
    (T.concat
       (map (\(E.Value t) -> t) status ++
        ["Database responded in " <> T.pack (show (diffUTCTime later now))]))

-- | Display some simple message in the home page.
getHomeR :: Handler Html
getHomeR = do
  dates <-
    runDB
      (E.select
         (E.from
            (\content -> do
               E.orderBy [E.desc (content E.^. ContentCreated)]
               E.limit 1
               pure (content E.^. ContentCreated, content E.^. ContentKey))))
  renderer <- getUrlRender
  pure
    (H.html
       (do H.head
             (do H.title "Casa"
                 H.style "body{font-family:sans-serif;}")
           H.body
             (do H.h1 "Casa"
                 H.h2 "Content-Addressable Storage Archive"
                 H.p "Statistics:"
                 H.ul
                   (do H.li
                         (do "Last uploaded blob: "
                             maybe
                               "Never"
                               (\(t, key) -> do
                                  H.strong (toHtml (show t))
                                  " "
                                  H.a !
                                    A.href
                                      (H.toValue (renderer (V1MetadataR key))) $
                                    H.code (toHtml (toPathPiece key)))
                               (fmap
                                  (\(E.Value t, E.Value key) -> (t, key))
                                  (listToMaybe dates))))
                 H.p (H.a ! A.href (H.toValue (renderer StatsR)) $ "More stats")
                 H.hr
                 H.p
                   (do "A service provided by "
                       H.a ! A.href "https://www.fpcomplete.com/" $
                         "FP Complete"))))

-- | Get some basic stats based on the logs.
getStatsR :: Handler Html
getStatsR = do
  do logs <-
       runDB
         (E.select
            (E.from
               (\log -> do
                  E.orderBy
                    [ E.desc (log E.^. AccessLogCount)
                    , E.desc (log E.^. AccessLogLastAccess)
                    ]
                  E.limit 100
                  pure log)))
     renderer <- getUrlRender
     pure
       (H.html
          (do H.head
                (do H.title "Casa stats"
                    H.style "body{font-family:sans-serif;}")
              H.body
                (do H.h1 "Casa stats"
                    H.table
                      (do H.thead
                            (do H.th "Key"
                                H.th "Pulls"
                                H.th "Last access")
                          H.tbody
                            (mapM_
                               (\((Entity _ log)) ->
                                  H.tr
                                    (do H.td
                                          (H.a !
                                           A.href
                                             (H.toValue
                                                (renderer
                                                   (V1MetadataR (accessLogKey log)))) $
                                           H.code (toHtml (toPathPiece (accessLogKey log))))
                                        H.td
                                          (toHtml (show (accessLogCount log)))
                                        H.td
                                          (toHtml
                                             (show (accessLogLastAccess log)))))
                               logs)))))

-- | Get a single blob in a web interface.
getV1MetadataR :: BlobKey -> Handler Value
getV1MetadataR key = do
  mblob <- runDB (selectFirst [ContentKey ==. key] [])
  case mblob of
    Nothing -> notFound
    Just (Entity _ blob) ->
      pure
        (object
           [ "key" .= contentKey blob
           , "created" .= contentCreated blob
           , "length" .= S.length (contentBlob blob)
           , "preview" .= show (S.take 80 (contentBlob blob))
           ])

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
  logAccesses (pure blobKey)
  case listToMaybe contents of
    Nothing -> notFound
    Just (E.Value bytes) ->
      pure
        (TypedContent
           "application/octet-stream"
           (ContentBuilder (S.byteString bytes) (Just (S.length bytes))))

-- | Push a batch of blobs.
postV1PushR :: Handler TypedContent
postV1PushR = do
  -- I take the time at the beginning of the request, this way it's
  -- easier to see which keys were uploaded at the same time.
  now <- liftIO getCurrentTime
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
                      (Content
                         { contentCreated = now
                         , contentKey = blobKey
                         , contentBlob = blob
                         })))))

-- | Pull a batch of blobs.
postV1PullR :: Handler TypedContent
postV1PullR = do
  keyLenPairs <- keyLenPairsFromBody
  let keys = fmap fst keyLenPairs
  logAccesses keys -- TODO: Put this in another thread later.
  let source =
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
       (\(E.Value blobKey, E.Value blob) ->
          [ Chunk (blobKeyToBuilder blobKey <> SB.byteString blob)
          , Flush -- Do we want to flush after every blob?
          ]))

--------------------------------------------------------------------------------
-- Access logger

-- | Log accesses of blob keys to the database.
logAccesses :: NonEmpty BlobKey -> Handler ()
logAccesses keys = do
  now <- liftIO getCurrentTime
  runDB
    (void
       (mapM_
          (\key ->
             upsertBy
               (AccessUnqiueKey key)
               (AccessLog
                  { accessLogKey = key
                  , accessLogCount = 1
                  , accessLogLastAccess = now
                  })
               [AccessLogLastAccess =. now, AccessLogCount +=. 1])
          keys))

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
    (filterLogger
       (\_src _lvl -> False)
       (withBackendPool (S8.pack dbstr) 10 cont))

--------------------------------------------------------------------------------
-- Hashing

-- | Hash some raw bytes.
sha256Hash :: ByteString -> ByteString
sha256Hash = Mem.convert . Crypto.hashWith Crypto.SHA256
