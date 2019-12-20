{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Copyright: (c) 2018-present Junyoung Clare Jang
-- License: BSD 3-Clause
module Minicute.Data.GMachine.Dump
  ( Dump
  , empty
  , saveState
  , loadState
  , extractAllAddresses

  , DumpItem
  , emptyDumpItem
  ) where

import Prelude hiding ( fail )

import Control.Lens.Getter ( use )
import Control.Lens.Operators
import Control.Lens.Operators.Minicute
import Control.Lens.TH
import Control.Lens.Traversal ( partsOf )
import Control.Lens.Tuple
import Control.Lens.Wrapped ( _Wrapped )
import Control.Monad.Extra ( concatMapM )
import Control.Monad.Fail
import Control.Monad.State ( MonadState, evalStateT )
import Data.Data ( Data, Typeable )
import GHC.Generics ( Generic )
import Minicute.Data.GMachine.Address

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
{-# INLINE empty #-}

saveState :: (MonadState s m, s ~ Dump) => DumpItem -> m ()
saveState di = _Wrapped %= (di :)
{-# INLINABLE saveState #-}

loadState :: (MonadState s m, s ~ Dump, MonadFail m) => m DumpItem
loadState = _Wrapped %%~= loadState'
  where
    loadState' (di : dis) = pure (di, dis)
    loadState' _ = fail "loadState: There is no dumped state to load"
    {-# INLINABLE loadState' #-}
{-# INLINABLE loadState #-}

extractAllAddresses :: (MonadState s m, s ~ Dump, MonadFail m) => m [Address]
extractAllAddresses = do
  addrStks <- use $ partsOf $ _Wrapped . traverse . _2
  concatMapM (evalStateT AddressStack.peekAllAddrs) addrStks
{-# INLINABLE extractAllAddresses #-}


emptyDumpItem :: DumpItem
emptyDumpItem
  = ( Code.empty
    , AddressStack.empty
    , ValueStack.empty
    )
{-# INLINE emptyDumpItem #-}
