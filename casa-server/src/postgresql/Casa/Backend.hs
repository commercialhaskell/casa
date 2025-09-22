{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeFamilies              #-}

-- |

module Casa.Backend
  ( module X
  , withBackendPool
  , manualMigration
  ) where

import           Control.Monad.Trans.Reader
import           Data.Functor
import           Database.Persist.Postgresql as X
import           Yesod

withBackendPool = withPostgresqlPool

manualMigration ::
     (MonadIO m, BackendCompatible SqlBackend backend)
  => ReaderT backend m ()
manualMigration =
  void (rawExecute
          "CREATE INDEX IF NOT EXISTS content_created_idx ON content(created);"
          [])
