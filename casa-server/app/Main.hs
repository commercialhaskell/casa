{-# LANGUAGE OverloadedStrings #-}

-- | Main entry point.

module Main where

import Casa.Server
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Pool
import Database.Persist.Postgresql
import Yesod

-- include migrateAll here

main :: IO ()
main = do
  withDBPool
    (\pool -> do
       withResource pool (runReaderT (runMigration migrateAll))
       liftIO (warpEnv (App {appPool = pool, appLogging = True})))
