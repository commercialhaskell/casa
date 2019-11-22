{-# LANGUAGE OverloadedStrings #-}

-- | Main entry point.

module Main where

import Casa.Server
import Control.Concurrent.Async
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Pool
import Casa.Backend
import System.Environment
import Yesod
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Gzip (gzip, def, gzipCheckMime)

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
    (\pool -> do
       withResource
         pool
         (runReaderT
            (do runMigration migrateAll
                manualMigration))
       liftIO
         (concurrently_
            (runWithAuthAndPort pool authorizedPort Authorized)
            (runWithAuthAndPort
               pool
               unauthorizedPort
               (Unauthorized "Not authorized on this port"))))
