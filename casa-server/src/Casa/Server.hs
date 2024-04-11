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
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

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
import           Control.Monad.Reader
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
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import           Data.Maybe
import           Data.Pool
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import           Data.Word
import qualified Database.Esqueleto as E
import           Lucid
import           Prelude hiding (log)
import           System.Environment
import           Yesod hiding (Content, Html, toHtml)
import           Yesod.Lucid

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
  /meta/#BlobKey MetaR GET
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
      MetaR {} -> pure Authorized
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
      Just MetaR {} -> Just maximumContentLen
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
getHomeR :: Handler (Html ())
getHomeR = do
  dates <-
    runDB
      (E.select
         (E.from
            (\content -> do
               E.orderBy [E.desc (content E.^. ContentCreated)]
               E.limit 1
               pure (content E.^. ContentCreated, content E.^. ContentKey))))
  htmlWithUrl
    (template
       Template
         { title = "Casa"
         , body =
             do h1_ "Casa"
                p_ (em_ "(Content-Addressable Storage Archive)")
                p_
                  (do "Last uploaded blob: "
                      maybe
                        "Never"
                        (\(t, key) -> do
                           strong_ (toHtml (show t))
                           " "
                           url <- lift ask
                           a_ [href_ (url (MetaR key))] $
                             code_ (toHtml (toPathPiece key)))
                        (fmap
                           (\(E.Value t, E.Value key) -> (t, key))
                           (listToMaybe dates)))
                url <- lift ask
                p_ (a_ [href_ (url StatsR)] "More statistics")
         })

-- | Get some basic stats based on the logs.
getStatsR :: Handler (Html ())
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
     htmlWithUrl
       (template
          Template
            { title = "Stats"
            , body =
                do h1_ "Casa stats"
                   table_
                     (do thead_
                           (do th_ "Key"
                               th_ "Pulls"
                               th_ "Last access")
                         tbody_
                           (mapM_
                              (\((Entity _ log)) ->
                                 tr_
                                   (do url <- lift ask
                                       td_
                                         (a_
                                            [ href_
                                                (url (MetaR (accessLogKey log)))
                                            ]
                                            (code_
                                               (toHtml
                                                  (toPathPiece
                                                     (accessLogKey log)))))
                                       td_ (toHtml (show (accessLogCount log)))
                                       td_
                                         (toHtml
                                            (show (accessLogLastAccess log)))))
                              logs))
            })

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
getMetaR :: BlobKey -> Handler (Html ())
getMetaR key = do
  mblob <- runDB (selectFirst [ContentKey ==. key] [])
  case mblob of
    Nothing -> notFound
    Just (Entity _ blob) -> do
      htmlWithUrl
        (template
           Template
             { title = "Meta"
             , body =
                 do url <- lift ask
                    h1_ (code_ (toHtml (toPathPiece key)))
                    p_ (a_ [href_ (url (KeyR key))] "Download raw")
                    p_
                      (do strong_ "Created: "
                          toHtml (show (contentCreated blob)))
                    p_
                      (do strong_ "Size: "
                          toHtml (show (S.length (contentBlob blob))))
                    p_ (strong_ "Preview (limited to 512 bytes)")
                    p_ (code_ (toHtml (show (S.take 512 (contentBlob blob)))))
             })

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
     (IsPersistBackend b, BaseBackend b ~ SqlBackend, b ~ SqlBackend)
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

--------------------------------------------------------------------------------
-- Minimal template

data Template m = Template
  { body :: HtmlT m ()
  , title :: Text
  }

template :: Monad m => Template m -> HtmlT m ()
template Template {body, title} =
  html_
    (do head_
          (do title_ (toHtml title)
              meta_
                [httpEquiv_ "Content-Type", content_ "text/html; charset=UTF-8"]
              meta_
                [ name_ "viewport"
                , content_ "width=device-width, initial-scale=1"
                ]
              style_
                "body {\
                \font-family: 'Lato', sans-serif; \
                \background: #f0f0f0;\
                \}\
                \a {\
                \color: #019a77;\
                \text-decoration: none;\
                \}\
                \h1, h2, h3, h4, h5 {\
                \color: #05685b;\
                \word-break: break-all;\
                \}\
                \code {\
                \white-space: pre-wrap;\
                \word-break: break-all;\
                \}\
                \hr { border: 1px solid #ccc; }")
        body_
          (do body
              hr_ []
              p_
                (do "A service created by "
                    a_ [href_ "https://www.fpcomplete.com/"] "FP Complete"
                    " in 2019 | Donated to the "
                    a_ [href_ "https://haskell.foundation"] "Haskell Foundation"
                    " in 2024")))
