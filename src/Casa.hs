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

module Casa
  ( App(App)
  , resourcesApp
  , Widget
  ) where

import           Control.Applicative
import           Control.Monad
import qualified Data.Attoparsec.ByteString as Atto.B
import qualified Data.Attoparsec.Text as Atto.T
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Builder as S
import           Data.Char
import           Data.Conduit
import           Data.Conduit.Attoparsec
import qualified Data.Conduit.List as CL
import           Data.Foldable
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Hashable
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import           Data.Maybe
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Yesod

--------------------------------------------------------------------------------
-- Constants

maxRequestableKeys :: Int
maxRequestableKeys = 1

--------------------------------------------------------------------------------
-- Types

-- | Server app.
data App = App

instance Yesod App where
  maximumContentLength _ _ = Just (1024 * 20)
  makeSessionBackend _ = return Nothing

-- | A blob of binary content.
newtype Blob =
  Blob
    { unBlob :: ByteString
    }
     deriving (Read, Eq, Show, IsString)

instance ToTypedContent Blob where
  toTypedContent = TypedContent "application/octet-stream" . toContent

instance ToContent Blob where
  toContent (Blob bytes) =
    ContentBuilder (S.byteString bytes) (Just (S.length bytes))

blobToBuilder :: Blob -> S.Builder
blobToBuilder = S.byteString . unBlob

-- | A key to address blob.
newtype BlobKey =
  BlobKey
    { unBlobKey :: ByteString
    }
  deriving (Read, Eq, Show, Ord, Hashable, IsString)

instance FromJSON BlobKey where
  parseJSON = parseJSON >=> (either fail pure . blobKeyParser)

instance PathPiece BlobKey where
  fromPathPiece =
    either (const Nothing) Just .
    blobKeyParser
  toPathPiece = T.decodeUtf8 . unBlobKey

-- | Parse a blob key.
blobKeyParser :: Text -> Either String BlobKey
blobKeyParser =
  Atto.T.parseOnly
    (fmap (BlobKey . T.encodeUtf8) (Atto.T.takeWhile isValidSha256Character))
  where
    isValidSha256Character c = isAscii c || isAlphaNum c

-- | Parse a blob key.
blobKeyParserBS :: Atto.B.Parser BlobKey
blobKeyParserBS = fmap BlobKey (Atto.B.take 32)

blobKeyToBuilder :: BlobKey -> S.Builder
blobKeyToBuilder = S.byteString . unBlobKey

--------------------------------------------------------------------------------
-- Routes

mkYesod "App" [parseRoutesNoCheck|
  /batch BatchBlobsR POST
  /#BlobKey SingleBlobR GET
|]

--------------------------------------------------------------------------------
-- Handlers

-- | Get a single blob in a web interface.
getSingleBlobR :: BlobKey -> Handler Blob
getSingleBlobR blobKey =
  case HM.lookup blobKey hardCodedKeys of
    Nothing -> notFound
    Just blob -> pure blob

-- | Get a batch of blobs.
postBatchBlobsR :: Handler TypedContent
postBatchBlobsR = do
  keys <- hashesFromBody
  -- We can later replace this with a call to a database.
  let results =
        mapMaybe
          (\key -> fmap (key, ) (HM.lookup key hardCodedKeys))
          (toList keys)
  -- We return a stream of key+blob pairs in binary format. The client
  -- knows how long the hash should be and how long the blob should be
  -- based on the hash.
  pure
    (TypedContent
       "application/octet-stream"
       (ContentSource
          (CL.sourceList
             (concatMap
                (\(blobKey, blob) ->
                   [ Chunk (blobKeyToBuilder blobKey <> blobToBuilder blob)
                     -- Do we want to flush after every file?
                   , Flush
                   ])
                results))))

--------------------------------------------------------------------------------
-- Hard-coded example keys

hardCodedKeys :: HashMap BlobKey Blob
hardCodedKeys =
  HM.fromList
    [ ( "334d016f755cd6dc58c53a86e183882f8ec14f52fb05345887c8a5edd42c87b7"
      , "Hello!")
    , ( "514b6bb7c846ecfb8d2d29ef0b5c79b63e6ae838f123da936fe827fda654276c"
      , "World!")
    ]

--------------------------------------------------------------------------------
-- Input reader

hashesFromBody ::
     (MonadHandler m)
  => m (NonEmpty BlobKey)
hashesFromBody = do
  do result <-
       runConduit
         (rawRequestBody .|
          sinkParserEither (many1 maxRequestableKeys blobKeyParserBS))
     case result of
       Left err ->
         invalidArgs
           ["Invalid blob keys, parse error: " <> T.pack (errorMessage err)]
       Right keys ->
         case NE.nonEmpty keys of
           Nothing -> invalidArgs ["No keys provided."]
           Just nonEmpty -> pure nonEmpty
  where
    many1 0 _ = fail "Max keys reached."
    many1 n m = do
      v <- fmap Just m <|> fmap (const Nothing) Atto.B.endOfInput
      case v of
        Nothing -> pure []
        Just x -> fmap (x :) (many1 (n - 1) m)
