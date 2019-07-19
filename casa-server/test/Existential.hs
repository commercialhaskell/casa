{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

-- | Existential tests.
module Main where

import           Casa.Client
import           Casa.Server
import           Casa.Types
import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad.Trans.Reader
import qualified Data.ByteString.Builder as SB
import           Data.Conduit
import           Data.Conduit.Attoparsec
import           Data.Conduit.ByteString.Builder
import qualified Data.Conduit.List as CL
import qualified Data.HashMap.Strict as HM
import           Data.Pool
import           Data.Text (Text)
import           Database.Persist.Postgresql
import qualified Network.BSD as Network
import qualified Network.Socket as Network
import qualified Network.Wai.Handler.Warp as Warp
import           Test.Hspec
import           Yesod

main :: IO ()
main = do
  withDBPool
    (\pool -> withResource pool (runReaderT (runMigration migrateAll)))
  hspec spec

spec :: SpecWith ()
spec = do
  inputSpec
  integrationSpec

inputSpec :: SpecWith ()
inputSpec =
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
-- Integration tests

integrationSpec :: SpecWith ()
integrationSpec = do
  describe
    "Push"
    (it
       "Push"
       (shouldReturn
          (do (port, runner) <-
                withDBPool
                  (\pool ->
                     liftIO
                       (runWarpOnFreePort
                          (App {appLogging = False, appPool = pool})))
              withAsync
                runner
                (const
                   (runConduitRes
                      (blobsSink
                         ("http://localhost:" ++ show port ++ "/v1/push")
                         (CL.sourceList ["Hello!", "World!"])))))
          ()))
  describe
    "Pull"
    (it
       "Pull"
       (shouldReturn
          (do (port, runner) <-
                withDBPool
                  (\pool ->
                     liftIO
                       (runWarpOnFreePort
                          (App {appLogging = False, appPool = pool})))
              withAsync
                runner
                (const
                   (runConduitRes
                      (blobsSource
                         ("http://localhost:" ++ show port ++ "/v1/pull")
                         (HM.fromList
                            [ ( partialKey
                                  "334d016f755cd6dc58c53a86e183882f8ec14f52fb05345887c8a5edd42c87b7"
                              , 6)
                            , ( partialKey
                                  "514b6bb7c846ecfb8d2d29ef0b5c79b63e6ae838f123da936fe827fda654276c"
                              , 6)
                            ]) .|
                       CL.consume))))
          [ ( partialKey
                "334d016f755cd6dc58c53a86e183882f8ec14f52fb05345887c8a5edd42c87b7"
            , "Hello!")
          , ( partialKey
                "514b6bb7c846ecfb8d2d29ef0b5c79b63e6ae838f123da936fe827fda654276c"
            , "World!")
          ]))

--------------------------------------------------------------------------------
-- Supplementary

-- | Orphan because ParseError doesn't provide it and the test suite needs it.
deriving instance Eq ParseError

-- | Makes a warp runner on an available port and returns the port it's running on.
runWarpOnFreePort :: (Yesod a, YesodDispatch a) => a -> IO (Int, IO ())
runWarpOnFreePort app = do
  socket <- listenOnLoopback
  port <- fmap fromIntegral (Network.socketPort socket)
  waiApp <- toWaiAppPlain app
  pure
    ( port
    , Warp.runSettingsSocket
        (Warp.setPort port Warp.defaultSettings)
        socket
        waiApp)

-- | Listen on the first available port.
listenOnLoopback :: IO Network.Socket
listenOnLoopback = do
  proto <- Network.getProtocolNumber "tcp"
  bracketOnError
    (Network.socket Network.AF_INET Network.Stream proto)
    Network.close
    (\sock -> do
       Network.setSocketOption sock Network.ReuseAddr 1
       address <- Network.getHostByName "127.0.0.1"
       Network.bind
         sock
         (Network.SockAddrInet Network.aNY_PORT (Network.hostAddress address))
       Network.listen sock Network.maxListenQueue
       return sock)

--------------------------------------------------------------------------------
-- Debugging/dev

-- | Make a partial key.
partialKey :: Text -> BlobKey
partialKey = either error id . blobKeyHexParser
