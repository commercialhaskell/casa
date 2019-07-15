-- | Main entry point.

module Main where

import Yesod
import Casa

main :: IO ()
main = warpEnv App
