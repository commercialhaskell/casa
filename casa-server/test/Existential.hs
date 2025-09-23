{-# OPTIONS_GHC -Wno-orphans      #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE Strict                   #-}

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
import qualified Data.Map.Strict as M
import           Data.Pool
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Distribution.License as Cabal
import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.PackageDescription.PrettyPrint as Cabal
import qualified Distribution.Version as Cabal
import qualified Network.BSD as Network
import qualified Network.Socket as Network
import qualified Network.Wai.Handler.Warp as Warp
import qualified Pantry
import qualified Pantry.SHA256 as Pantry
import           RIO hiding ( bracketOnError, race, withAsync )
import           RIO.PrettyPrint.StylesUpdate
import           RIO.Process
import qualified Stack.Build.Target
import qualified Stack.Config
import qualified Stack.Prelude
import qualified Stack.Runners
import qualified Stack.Types.BuildOptsCLI
import qualified Stack.Types.ConfigMonoid
import qualified Stack.Types.GlobalOpts
import qualified Stack.Types.LockFileBehavior
import qualified Stack.Types.Runner
import qualified Stack.Types.Snapshot
import qualified Stack.Types.StackYamlLoc
import           Test.Hspec
import           Yesod

main :: IO ()
main = do
  withDBPool $ \pool ->
    liftIO $ withResource pool (runReaderT (runMigration migrateAll))
  hspec spec

spec :: SpecWith ()
spec = do
  inputSpec
  integrationSpec
  stackSpec

inputSpec :: SpecWith ()
inputSpec =
  describe "Input parsing" $ do
    it "Hash from stream" $
      shouldReturn
        ( runConduit
            (  yield
                 (blobKeyToBuilder
                    (partialKey
                       "334d016f755cd6dc58c53a86e183882f8ec14f52fb05345887c8a5edd42c87b7") <>
                  SB.word64BE 1234)
            .| builderToByteString
            .| hashesFromStream
            )
        )
        ( Right
            [ ( partialKey
                  "334d016f755cd6dc58c53a86e183882f8ec14f52fb05345887c8a5edd42c87b7"
              , 1234
              )
            ]
        )
    it "Hashes from stream" $
      shouldReturn
        ( runConduit
            (  yield
                 ( mconcat
                     [ blobKeyToBuilder
                         (partialKey
                            "334d016f755cd6dc58c53a86e183882f8ec14f52fb05345887c8a5edd42c87b7") <>
                       SB.word64BE 1234
                     , blobKeyToBuilder
                         (partialKey
                            "514b6bb7c846ecfb8d2d29ef0b5c79b63e6ae838f123da936fe827fda654276c") <>
                       SB.word64BE 5678
                     ]
                 )
            .| builderToByteString
            .| hashesFromStream
            )
        )
        ( Right
            [ ( partialKey
                  "334d016f755cd6dc58c53a86e183882f8ec14f52fb05345887c8a5edd42c87b7"
              , 1234
              )
            , ( partialKey
                  "514b6bb7c846ecfb8d2d29ef0b5c79b63e6ae838f123da936fe827fda654276c"
              , 5678
              )
            ]
        )

--------------------------------------------------------------------------------
-- Integration tests

integrationSpec :: SpecWith ()
integrationSpec = do
  describe "Push" $
    it "Push" $
      shouldReturn
        ( do (port, runner) <-
               withDBPool $ \pool ->
                 liftIO $ runWarpOnFreePort
                    (App
                       { appAuthorized = Authorized
                       , appLogging = False
                       , appPool = pool
                       }
                    )
             repo <-
               either
                 error
                 pure
                 (parseCasaRepoPrefix ("http://localhost:" ++ show port))
             withAsync
               runner
               (const (blobsSink repo (CL.sourceList ["Hello!", "World!"])))
        )
        ()
  describe "Pull" $
    sequence_
      [ it ("Pull " ++ "(max per request: " ++ show maxPerRequest ++ ")") $
          shouldReturn
            ( do (port, runner) <-
                   withDBPool $ \pool ->
                     liftIO $ runWarpOnFreePort
                        ( App
                            { appAuthorized =
                                Unauthorized "Not needed in this test"
                            , appLogging = False
                            , appPool = pool
                            }
                        )
                 repo <-
                   either
                     error
                     pure
                     (parseCasaRepoPrefix ("http://localhost:" ++ show port))
                 withAsync
                   runner
                   ( const
                       ( runConduitRes
                           ( blobsSource
                                  ( SourceConfig
                                      { sourceConfigUrl = repo
                                      , sourceConfigBlobs =
                                          HM.fromList
                                             [ ( partialKey
                                                   "334d016f755cd6dc58c53a86e183882f8ec14f52fb05345887c8a5edd42c87b7"
                                               , 6
                                               )
                                             , ( partialKey
                                                   "514b6bb7c846ecfb8d2d29ef0b5c79b63e6ae838f123da936fe827fda654276c"
                                               , 6
                                               )
                                             ]
                                      , sourceConfigMaxBlobsPerRequest = maxPerRequest
                                      }
                                  )
                              .| fmap HM.fromList CL.consume
                          )
                        )
                   )
            )
            ( HM.fromList
                [ ( partialKey
                      "334d016f755cd6dc58c53a86e183882f8ec14f52fb05345887c8a5edd42c87b7"
                  , "Hello!"
                  )
                , ( partialKey
                      "514b6bb7c846ecfb8d2d29ef0b5c79b63e6ae838f123da936fe827fda654276c"
                  , "World!"
                  )
                ]
            )
      | maxPerRequest <- [1 .. 3]
      ]

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
  , genericPackageDescription :: Cabal.GenericPackageDescription
  }

stackSpec :: Spec
stackSpec =
  describe "Stack" $
    it "Spin up a server with one package in it, pull the package with Stack"
      stackPullTest

-- | Check that running a Casa server with a randomly-generated
-- package in it, and downloading it with Stack works correctly.
stackPullTest :: IO ()
stackPullTest = do
  fabricated@FabricatedPackage {packageLocationImmutable, treeKey, genericPackageDescription} <-
    fmap makePackageLocationImmutable UUID.nextRandom
  (port, runWebServer) <- makeRunCasaForStack fabricated
  result <-
    race runWebServer (runStackAgainstOurCasa port packageLocationImmutable)
  shouldBe
    (fmap (second Pantry.packageTreeKey) result)
    (Right (genericPackageDescription, treeKey))

-- | Make a local, ephemeral, Casa deploy with a single fabricated
-- package inside it.
makeRunCasaForStack :: FabricatedPackage -> IO (Int, IO ())
makeRunCasaForStack
  FabricatedPackage
    { cabalFileBlobKey = Pantry.BlobKey cabalFileSha256 _
    , cabalFileBytes
    , treeKey = Pantry.TreeKey (Pantry.BlobKey treeSha256 _)
    , packageTree
    } =
  withDBPool $ \pool ->
    liftIO $ do
      now <- getCurrentTime
      withResource
        pool
        ( runReaderT $ do
            void
              ( insertUnique
                  Content
                    { contentKey =
                        BlobKey (Pantry.toRaw cabalFileSha256)
                    , contentBlob = cabalFileBytes
                    , contentCreated = now
                    }
              )
            void
              ( insertUnique
                  Content
                    { contentKey = BlobKey (Pantry.toRaw treeSha256)
                    , contentBlob = Pantry.renderTree packageTree
                    , contentCreated = now
                    }
              )
        )
      runWarpOnFreePort
        ( App
            { appAuthorized = Unauthorized "Not needed in this test"
            , appLogging = True
            , appPool = pool
            }
        )

-- | Run Stack and load the package against the local Casa server.
runStackAgainstOurCasa ::
     Int
  -> Pantry.PackageLocationImmutable
  -> IO (Cabal.GenericPackageDescription, Pantry.Package)
runStackAgainstOurCasa port packageLocationImmutable = do
  case parseCasaRepoPrefix ("http://localhost:" <> show port) of
    Left err -> error ("Casa repo parse error: " ++ err)
    Right casaRepoPrefix ->
      runSimpleApp $ do
        logFunc <- RIO.asks (view logFuncL)
        processContext <- RIO.asks (view processContextL)
        dockerEntrypointMVar <- newMVar False
        runRIO
          ( Stack.Types.Runner.Runner
              { globalOpts = runnerGlobalOpts casaRepoPrefix
              , useColor = False
              , logFunc
              , termWidth = 80
              , processContext
              , dockerEntrypointMVar
              }
          )
          ( Stack.Config.loadConfig $ \config ->
              runRIO
                config
                ( Stack.Runners.withEnvConfig
                    Stack.Build.Target.AllowNoTargets
                    Stack.Types.BuildOptsCLI.defaultBuildOptsCLI
                    ( do cabalDesc <-
                           Pantry.loadCabalFileImmutable
                             packageLocationImmutable
                         package <- Pantry.loadPackage packageLocationImmutable
                         pure (cabalDesc, package)
                    )
                )
          )
  where
    runnerGlobalOpts casaRepoPrefix =
      Stack.Types.GlobalOpts.GlobalOpts
        { reExecVersion = Nothing
        , dockerEntrypoint = Nothing
        , logLevel = RIO.LevelInfo
        , timeInLog = False
        , rslInLog = False
        , planInLog = False
        , configMonoid =
            mempty
              { Stack.Types.ConfigMonoid.casaRepoPrefix =
                  pure casaRepoPrefix
              }
        , snapshot =
            Just $
              Stack.Types.Snapshot.ASSnapshot $
                Stack.Prelude.RSLSynonym $
                  Stack.Prelude.LTS 23 24
                 -- This matches the lts-23.24 used by Stack 3.7.1. It's not
                 -- particularly important, as our test crosses snapshot
                 -- boundaries.
        , compiler = Nothing
        , terminal = False
        , stylesUpdate = StylesUpdate []
        , termWidthOpt = Nothing
        , stackYaml = Stack.Types.StackYamlLoc.SYLNoProject []
        , lockFileBehavior = Stack.Types.LockFileBehavior.LFBIgnore
        , progName = ""
        , mExecutablePath = Nothing
        }

-- | Make a fake package.
makePackageLocationImmutable :: UUID.UUID -> FabricatedPackage
makePackageLocationImmutable uuid =
  FabricatedPackage
    { packageLocationImmutable
    , cabalFileBytes
    , cabalFileBlobKey
    , packageTree
    , treeKey
    , genericPackageDescription
    }
  where
    packageLocationImmutable =
      Pantry.PLIHackage cabalPackageIdentifier cabalFileBlobKey treeKey
    cabalFilePath =
      case Pantry.mkSafeFilePath (fromString pkgName <> ".cabal") of
        Nothing ->
          error
            "makePackageLocationImmutable: Invalid file path. This is unexpected!"
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
        ( M.fromList
            [ ( cabalFilePath
              , Pantry.TreeEntry
                  { teBlob = cabalFileBlobKey
                  , teType = Pantry.FTNormal
                  }
              )
            ]
        )
    cabalFileBytes =
      T.encodeUtf8
        (T.pack (Cabal.showGenericPackageDescription genericPackageDescription))
    cabalPackageIdentifier =
      Cabal.PackageIdentifier {pkgName = fromString pkgName, pkgVersion}
    genericPackageDescription =
      Cabal.emptyGenericPackageDescription
        { Cabal.packageDescription =
            Cabal.emptyPackageDescription
              { Cabal.package = cabalPackageIdentifier
              , Cabal.licenseRaw =
                  Right (Cabal.UnknownLicense "UnspecifiedLicense")
              }
        }
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
        waiApp
    )

-- | Listen on the first available port.
listenOnLoopback :: IO Network.Socket
listenOnLoopback = do
  proto <- Network.getProtocolNumber "tcp"
  bracketOnError
    (Network.socket Network.AF_INET Network.Stream proto)
    Network.close
    ( \sock -> do
         Network.setSocketOption sock Network.ReuseAddr 1
         address <- Network.getHostByName "127.0.0.1"
         Network.bind
           sock
           ( Network.SockAddrInet
               Network.defaultPort
               (Network.hostAddress address)
           )
         Network.listen sock Network.maxListenQueue
         return sock
    )

--------------------------------------------------------------------------------
-- Debugging/dev

-- | Make a partial key.
partialKey :: Text -> BlobKey
partialKey = either error id . blobKeyHexParser
