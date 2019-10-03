{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Minicute.Data.GMachine.Dump
  ( Dump
  , empty
  , saveState
  , loadState

  , DumpItem
  , emptyDumpItem
  ) where

import Prelude hiding ( fail )

import Control.Lens.Operators
import Control.Lens.Operators.Minicute
import Control.Lens.TH
import Control.Lens.Wrapped ( _Wrapped )
import Control.Monad.Fail
import Control.Monad.State ( MonadState )
import Data.Data
import GHC.Generics

import qualified Minicute.Data.GMachine.AddressStack as AddressStack
import qualified Minicute.Data.GMachine.Code as Code
import qualified Minicute.Data.GMachine.ValueStack as ValueStack

newtype Dump
  = Dump [DumpItem]
  deriving ( Generic
           , Typeable
           , Data
           , Eq
           , Ord
           , Show
           )

type DumpItem = (Code.Code, AddressStack.AddressStack, ValueStack.ValueStack)

makeWrapped ''Dump

empty :: Dump
empty = Dump []

saveState :: (MonadState s m, s ~ Dump) => DumpItem -> m ()
saveState di = _Wrapped %= (di :)

loadState :: (MonadState s m, s ~ Dump, MonadFail m) => m DumpItem
loadState = _Wrapped %%~= loadState'
  where
    loadState' (di : dis) = pure (di, dis)
    loadState' _ = fail "loadState: There is no dumped state to load"


emptyDumpItem :: DumpItem
emptyDumpItem
  = ( Code.empty
    , AddressStack.empty
    , ValueStack.empty
    )
