{-# HLINT ignore "Use fewer imports"      #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Main entry point.

module Main where

import Casa.Server ( App (..), migrateAll, withDBPool )
import Control.Concurrent.Async ( concurrently_ )
import Control.Monad.IO.Class ( liftIO )
import Control.Monad.Trans.Reader ( runReaderT )
import Data.Pool ( withResource )
import Casa.Backend ( manualMigration, runMigration )
import System.Environment ( getEnv )
import Yesod ( AuthResult (..), toWaiApp )
import Network.Wai.Handler.Warp ( run )
import Network.Wai.Middleware.Gzip ( gzip, gzipCheckMime )
#if MIN_VERSION_wai_extra(3,1,14)
import Network.Wai.Middleware.Gzip ( GzipSettings, defaultGzipSettings )
#else
import Network.Wai.Middleware.Gzip ( def )
#endif

#if  MIN_VERSION_wai_extra(3,1,14)
def :: GzipSettings
def = defaultGzipSettings
#endif

-- | Run two servers on different ports, accessing the same app, but
-- with a different 'appAuthorized' flag. They share the same database
-- pool.
main :: IO ()
main = do
  unauthorizedPort <- fmap read (getEnv "PORT")
  authorizedPort <- fmap read (getEnv "AUTHORIZED_PORT")
  let runWithAuthAndPort pool port authorized = do
        let yapp =
              App
                {appAuthorized = authorized, appPool = pool, appLogging = True}
        app <- toWaiApp yapp
        run port (gzip def {gzipCheckMime = const True} app)
  withDBPool
    ( \pool -> liftIO $ do
        withResource
          pool
          ( runReaderT $ do
              runMigration migrateAll
              manualMigration
          )
        concurrently_
          ( runWithAuthAndPort
              pool
              authorizedPort
              Authorized
          )
          ( runWithAuthAndPort
              pool
              unauthorizedPort
              (Unauthorized "Not authorized on this port")
          )
    )
