{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

-- |

module Casa.Backend
  ( module X
  , withBackendPool
  , manualMigration
  ) where

import qualified Data.Text.Encoding as T
import           Database.Persist.Sqlite as X

withBackendPool string = withSqlitePool (T.decodeUtf8 string)

manualMigration :: Applicative f => f ()
manualMigration = pure ()
