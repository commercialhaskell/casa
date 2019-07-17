{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Existential tests.
module Main where

import           Casa.Server
import           Casa.Types
import qualified Data.ByteString.Builder as SB
import           Data.Conduit
import           Data.Conduit.Attoparsec
import           Data.Conduit.ByteString.Builder
import           Data.Text (Text)
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: SpecWith ()
spec =
  describe
    "Input parsing"
    (do it
          "Hash from stream"
          (shouldReturn
             (runConduit
                (yield
                   (blobKeyToBuilder
                      (partialKey
                         "334d016f755cd6dc58c53a86e183882f8ec14f52fb05345887c8a5edd42c87b7") <>
                    SB.word64BE 1234) .|
                 builderToByteString .|
                 hashesFromStream))
             (Right
                [ ( partialKey
                      "334d016f755cd6dc58c53a86e183882f8ec14f52fb05345887c8a5edd42c87b7"
                  , 1234)
                ]))
        it
          "Hashes from stream"
          (shouldReturn
             (runConduit
                (yield
                   (mconcat
                      [ blobKeyToBuilder
                          (partialKey
                             "334d016f755cd6dc58c53a86e183882f8ec14f52fb05345887c8a5edd42c87b7") <>
                        SB.word64BE 1234
                      , blobKeyToBuilder
                          (partialKey
                             "514b6bb7c846ecfb8d2d29ef0b5c79b63e6ae838f123da936fe827fda654276c") <>
                        SB.word64BE 5678
                      ]) .|
                 builderToByteString .|
                 hashesFromStream))
             (Right
                [ ( partialKey
                      "334d016f755cd6dc58c53a86e183882f8ec14f52fb05345887c8a5edd42c87b7"
                  , 1234)
                , ( partialKey
                      "514b6bb7c846ecfb8d2d29ef0b5c79b63e6ae838f123da936fe827fda654276c"
                  , 5678)
                ])))

--------------------------------------------------------------------------------
-- Supplementary

-- | Orphan because ParseError doesn't provide it and the test suite needs it.
deriving instance Eq ParseError

--------------------------------------------------------------------------------
-- Debugging/dev

-- | Make a partial key.
partialKey :: Text -> BlobKey
partialKey = either error id . blobKeyHexParser
{-# DEPRECATED partialKey "This is just for debugging." #-}
