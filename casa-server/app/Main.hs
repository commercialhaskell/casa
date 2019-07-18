-- | Main entry point.

module Main where

import Yesod
import Casa.Server

main :: IO ()
main = warpEnv App {appLogging = True}
