{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Types and functions for use with Casa (Content-Addressable Storage
-- Archive). See <https://casa.stackage.org/>.

module Casa.Types
  ( BlobKey (..)
  , blobKeyHexParser
  , blobKeyBinaryParser
  , blobKeyToBuilder
  ) where

import           Control.Monad ( (>=>) )
import           Data.Aeson ( FromJSON (..), ToJSON (..), Value (..) )
import qualified Data.Attoparsec.ByteString as Atto.B
import qualified Data.Attoparsec.Text as Atto.T
import           Data.ByteString ( ByteString )
import qualified Data.ByteString.Base16 as Hex
import qualified Data.ByteString.Builder as S
import           Data.Hashable ( Hashable )
import           Data.Text ( Text )
import qualified Data.Text.Encoding as T
import           Database.Persist ( PersistField )
import           Database.Persist.Sql ( PersistFieldSql )
import           Web.PathPieces ( PathPiece (..) )

-- | A SHA256 key to address blobs.
newtype BlobKey =
  BlobKey
    { unBlobKey :: ByteString
    }
  deriving (Eq, Hashable, PersistField, PersistFieldSql, Ord, Read)

instance Show BlobKey where

  show (BlobKey key) = show (Hex.encode key)

instance FromJSON BlobKey where

  parseJSON = parseJSON >=> (either fail pure . blobKeyHexParser)

instance ToJSON BlobKey where

  toJSON = String . T.decodeUtf8 . Hex.encode . unBlobKey

instance PathPiece BlobKey where

  fromPathPiece = either (const Nothing) Just . blobKeyHexParser

  toPathPiece = T.decodeUtf8 . Hex.encode . unBlobKey

-- | Parse a blob key in hex format.
blobKeyHexParser :: Text -> Either String BlobKey
blobKeyHexParser =
  Atto.T.parseOnly $
    BlobKey <$> do
      bytes <- Atto.T.take 64
      case Hex.decode (T.encodeUtf8 bytes) of
        Right result -> pure result
        Left _ -> fail "Invalid hex key."

-- | Parse a blob key in binary format.
blobKeyBinaryParser :: Atto.B.Parser BlobKey
blobKeyBinaryParser = BlobKey <$> Atto.B.take 32

-- | Yield a t'S.Builder' value corresponding to the given t'BlobKey' value.
blobKeyToBuilder :: BlobKey -> S.Builder
blobKeyToBuilder = S.byteString . unBlobKey
