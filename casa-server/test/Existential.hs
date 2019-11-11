{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

-- | Existential tests.
module Main where

import           Casa.Backend
import           Casa.Client
import           Casa.Server
import           Casa.Types
import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad.Trans.Reader
import qualified Data.ByteString as S
import qualified Data.ByteString.Builder as SB
import           Data.Conduit
import           Data.Conduit.Attoparsec
import           Data.Conduit.ByteString.Builder
import qualified Data.Conduit.List as CL
import qualified Data.HashMap.Strict as HM
import           Data.List
import qualified Data.Map.Strict as M
import           Data.Pool
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Distribution.Types.PackageId as Cabal
import qualified Distribution.Version as Cabal
import qualified Network.BSD as Network
import qualified Network.Socket as Network
import qualified Network.Wai.Handler.Warp as Warp
import qualified Pantry as Pantry
import qualified Pantry.Internal as Pantry
import qualified Pantry.SHA256 as Pantry
import           RIO hiding (bracketOnError, withAsync, race_)
import           RIO.PrettyPrint.StylesUpdate
import           RIO.Process
import qualified Stack.Build.Target
import qualified Stack.Config
import qualified Stack.Runners
import qualified Stack.Types.Config
import qualified Stack.Types.Config.Build
import qualified Stack.Types.Resolver
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
  stackSpec

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
                          (App
                             { appAuthorized = Authorized
                             , appLogging = False
                             , appPool = pool
                             })))
              repo <-
                either
                  error
                  pure
                  (parseCasaRepoPrefix ("http://localhost:" ++ show port))
              withAsync
                runner
                (const (blobsSink repo (CL.sourceList ["Hello!", "World!"]))))
          ()))
  describe
    "Pull"
    (sequence_
       [ it
         ("Pull " ++ "(max per request: " ++ show maxPerRequest ++ ")")
         (shouldReturn
            (do (port, runner) <-
                  withDBPool
                    (\pool ->
                       liftIO
                         (runWarpOnFreePort
                            (App
                               { appAuthorized =
                                   Unauthorized "Not needed in this test"
                               , appLogging = False
                               , appPool = pool
                               })))
                repo <-
                  either
                    error
                    pure
                    (parseCasaRepoPrefix ("http://localhost:" ++ show port))
                withAsync
                  runner
                  (const
                     (runConduitRes
                        (blobsSource
                           (SourceConfig
                              { sourceConfigUrl = repo
                              , sourceConfigBlobs =
                                  (HM.fromList
                                     [ ( partialKey
                                           "334d016f755cd6dc58c53a86e183882f8ec14f52fb05345887c8a5edd42c87b7"
                                       , 6)
                                     , ( partialKey
                                           "514b6bb7c846ecfb8d2d29ef0b5c79b63e6ae838f123da936fe827fda654276c"
                                       , 6)
                                     ])
                              , sourceConfigMaxBlobsPerRequest = maxPerRequest
                              }) .|
                         fmap HM.fromList CL.consume))))
            (HM.fromList
               [ ( partialKey
                     "334d016f755cd6dc58c53a86e183882f8ec14f52fb05345887c8a5edd42c87b7"
                 , "Hello!")
               , ( partialKey
                     "514b6bb7c846ecfb8d2d29ef0b5c79b63e6ae838f123da936fe827fda654276c"
                 , "World!")
               ]))
       | maxPerRequest <- [1 .. 3]
       ])

--------------------------------------------------------------------------------
-- Tests for Stack
--
-- We're currently testing stack against Casa, rather than vice-versa,
-- until Stack's snapshot is brought forward.

data FabricatedPackage = FabricatedPackage
  { packageLocationImmutable :: Pantry.PackageLocationImmutable
  , cabalFileBytes :: ByteString
  , cabalFileBlobKey :: Pantry.BlobKey
  , packageTree :: Pantry.Tree
  , treeKey :: Pantry.TreeKey
  }

stackSpec :: Spec
stackSpec =
  describe
    "Stack"
    (it
       "Spin up a server with one package in it, pull the package with Stack"
       stackPullTest)

stackPullTest :: IO ()
stackPullTest = do
  fabricated@FabricatedPackage {packageLocationImmutable} <-
    fmap makePackageLocationImmutable UUID.nextRandom
  (port, runWebServer) <- makeRunCasaForStack fabricated
  race_ runWebServer (runStackAgainstOurCasa port packageLocationImmutable)

makeRunCasaForStack :: FabricatedPackage -> IO (Int, IO ())
makeRunCasaForStack FabricatedPackage { cabalFileBlobKey = Pantry.BlobKey cabalFileSha256 _
                                      , cabalFileBytes
                                      , treeKey = Pantry.TreeKey (Pantry.BlobKey treeSha256 _)
                                      , packageTree
                                      } =
  withDBPool
    (\pool ->
       liftIO
         (do now <- getCurrentTime
             withResource
               pool
               (runReaderT
                  (do void
                        (insertUnique
                           Content
                             { contentKey =
                                 BlobKey (Pantry.toRaw cabalFileSha256)
                             , contentBlob = cabalFileBytes
                             , contentCreated = now
                             })
                      void
                        (insertUnique
                           Content
                             { contentKey = BlobKey (Pantry.toRaw treeSha256)
                             , contentBlob = Pantry.renderTree packageTree
                             , contentCreated = now
                             })))
             runWarpOnFreePort
               (App
                  { appAuthorized = Unauthorized "Not needed in this test"
                  , appLogging = True
                  , appPool = pool
                  })))

runStackAgainstOurCasa :: Int -> Pantry.PackageLocationImmutable -> IO ()
runStackAgainstOurCasa port packageLocationImmutable = do
  case parseCasaRepoPrefix ("http://localhost:" <> show port) of
    Left err -> error ("Casa repo parse error: " ++ err)
    Right casaRepoPrefix ->
      runSimpleApp
        (do runnerLogFunc <- RIO.asks (view logFuncL)
            runnerProcessContext <- RIO.asks (view processContextL)
            runRIO
              (Stack.Types.Config.Runner
                 { runnerGlobalOpts = runnerGlobalOpts casaRepoPrefix
                 , runnerUseColor = False
                 , runnerLogFunc
                 , runnerTermWidth = 80
                 , runnerProcessContext
                 })
              (Stack.Config.loadConfig
                 (\config ->
                    runRIO
                      config
                      (Stack.Runners.withEnvConfig
                         Stack.Build.Target.AllowNoTargets
                         Stack.Types.Config.Build.defaultBuildOptsCLI
                         (do cabalDesc <-
                               Pantry.loadCabalFileImmutable
                                 packageLocationImmutable
                             RIO.logInfo (fromString (show cabalDesc))
                             package <- Pantry.loadPackage packageLocationImmutable
                             RIO.logInfo (fromString (show package)))))))
  where
    runnerGlobalOpts casaRepoPrefix =
      Stack.Types.Config.GlobalOpts
        { globalReExecVersion = Nothing
        , globalDockerEntrypoint = Nothing
        , globalLogLevel = RIO.LevelInfo
        , globalTimeInLog = False
        , globalConfigMonoid =
            mempty
              { Stack.Types.Config.configMonoidCasaRepoPrefix =
                  pure casaRepoPrefix
              }
        , globalResolver = Just Stack.Types.Resolver.ARGlobal
        , globalCompiler = Nothing
        , globalTerminal = False
        , globalStylesUpdate = StylesUpdate []
        , globalTermWidth = Nothing
        , globalStackYaml = Stack.Types.Config.SYLNoProject []
        , globalLockFileBehavior = Stack.Types.Config.LFBIgnore
        }

makePackageLocationImmutable :: UUID.UUID -> FabricatedPackage
makePackageLocationImmutable uuid =
  FabricatedPackage
    { packageLocationImmutable
    , cabalFileBytes
    , cabalFileBlobKey
    , packageTree
    , treeKey
    }
  where
    packageLocationImmutable =
      Pantry.PLIHackage
        (Cabal.PackageIdentifier {pkgName = fromString pkgName, pkgVersion})
        cabalFileBlobKey
        treeKey
    cabalFilePath =
      case Pantry.mkSafeFilePath (fromString pkgName <> ".cabal") of
        Nothing ->
          error
            ("makePackageLocationImmutable: Invalid file path. This is unexpected!")
        Just fp -> fp
    pkgName = "dummy" <> filter (/= '-') (UUID.toString uuid)
    pkgVersion = Cabal.mkVersion [0]
    treeKey =
      Pantry.TreeKey
        (Pantry.BlobKey
           (Pantry.hashBytes treeRendered)
           (Pantry.FileSize (fromIntegral (S.length treeRendered))))
    treeRendered = Pantry.renderTree packageTree
    packageTree =
      Pantry.TreeMap
        (M.fromList
           [ ( cabalFilePath
             , Pantry.TreeEntry
                 {teBlob = cabalFileBlobKey, teType = Pantry.FTNormal})
           ])
    cabalFileBytes =
      T.encodeUtf8
        (T.unlines
           [ "name: " <> fromString pkgName
           , "version: " <>
             fromString
               (intercalate "." (map show (Cabal.versionNumbers pkgVersion)))
           , "build-type: Simple"
           , "cabal-version: >= 1.2"
           , "library"
           ])
    cabalFileBlobKey =
      Pantry.BlobKey
        (Pantry.hashBytes cabalFileBytes)
        (Pantry.FileSize (fromIntegral (S.length cabalFileBytes)))

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
