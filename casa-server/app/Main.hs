{-# LANGUAGE OverloadedStrings #-}

-- | Main entry point.

module Main where

import           Casa.Server
import           Control.Monad.IO.Class
import           Yesod

main :: IO ()
main = do
  withDBPool
    (\pool -> liftIO (warpEnv (App {appPool = pool, appLogging = True})))
