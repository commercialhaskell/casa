{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
-- |

module Casa.Backend (module X, withBackendPool) where

import           Database.Persist.Postgresql as X

withBackendPool = withPostgresqlPool
