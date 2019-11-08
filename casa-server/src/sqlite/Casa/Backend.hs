{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
-- |

module Casa.Backend (module X, withBackendPool) where

import qualified Data.Text.Encoding as T
import           Database.Persist.Sqlite as X

withBackendPool string = withSqlitePool (T.decodeUtf8 string)
