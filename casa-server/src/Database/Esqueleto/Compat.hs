{-# LANGUAGE CPP #-}

{- |
This module avoids the use of CPP in module "Casa.Server".
-}

module Database.Esqueleto.Compat
  ( module E
  ) where

#if MIN_VERSION_esqueleto(3,5,0)
import           Database.Esqueleto.Legacy as E
#else
import           Database.Esqueleto as E
#endif
