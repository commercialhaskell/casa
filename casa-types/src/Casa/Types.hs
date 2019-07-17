{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |

module Casa.Types where

import           Control.Monad
import           Data.Aeson
import qualified Data.Attoparsec.ByteString as Atto.B
import qualified Data.Attoparsec.Text as Atto.T
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Base16 as Hex
import qualified Data.ByteString.Builder as S
import           Data.Hashable
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import           Web.PathPieces

-- | A SHA256 key to address blobs.
newtype BlobKey =
  BlobKey
    { unBlobKey :: ByteString
    }
  deriving (Read, Eq, Ord, Hashable)

instance Show BlobKey where
  show (BlobKey key) = show (Hex.encode key)

instance FromJSON BlobKey where
  parseJSON = parseJSON >=> (either fail pure . blobKeyHexParser)

instance PathPiece BlobKey where
  fromPathPiece =
    either (const Nothing) Just .
    blobKeyHexParser
  toPathPiece = T.decodeUtf8 . unBlobKey

-- | Parse a blob key in hex format.
blobKeyHexParser :: Text -> Either String BlobKey
blobKeyHexParser =
  Atto.T.parseOnly
    (fmap
       BlobKey
       (do bytes <- Atto.T.take 64
           case Hex.decode (T.encodeUtf8 bytes) of
             (result, wrong) | S.null wrong -> pure result
             _ -> fail "Invalid hex key."))

-- | Parse a blob key in binary format.
blobKeyBinaryParser :: Atto.B.Parser BlobKey
blobKeyBinaryParser = fmap BlobKey (Atto.B.take 32)

blobKeyToBuilder :: BlobKey -> S.Builder
blobKeyToBuilder = S.byteString . unBlobKey
