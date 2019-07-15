{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- |

module Casa (App(App)) where

import qualified Data.Attoparsec.Text as Atto
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Builder as S
import           Data.Char
import           Data.Either
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Hashable
import           Data.Maybe
import           Data.String
import qualified Data.Text.Encoding as T
import           Yesod

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

-- | A key to address blob.
newtype BlobKey =
  BlobKey
    { unBlobKey :: ByteString
    }
  deriving (Read, Eq, Show, Ord, Hashable, IsString)

instance PathPiece BlobKey where
  fromPathPiece =
    either (const Nothing) Just .
    Atto.parseOnly
      (fmap (BlobKey . T.encodeUtf8) (Atto.takeWhile isValidSha256Character))
    where
      isValidSha256Character c = isAscii c || isAlphaNum c
  toPathPiece = T.decodeUtf8 . unBlobKey

--------------------------------------------------------------------------------
-- Routes

mkYesod "App" [parseRoutes|
  /#BlobKey SingleBlobR GET
|]

--------------------------------------------------------------------------------
-- Handlers

getSingleBlobR :: BlobKey -> Handler Blob
getSingleBlobR blobKey =
  case HM.lookup blobKey hardCodedKeys of
    Nothing -> notFound
    Just blob -> pure blob

--------------------------------------------------------------------------------
-- Hard-coded example keys

hardCodedKeys :: HashMap BlobKey Blob
hardCodedKeys =
  HM.fromList
    [ ( "334d016f755cd6dc58c53a86e183882f8ec14f52fb05345887c8a5edd42c87b7"
      , "Hello!")
    ]
