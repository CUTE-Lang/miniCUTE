{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

  , getAllDumpItems

  , DumpItem
  , emptyDumpItem
  ) where

import Control.Lens.Getter ( use )
import Control.Lens.Iso ( Iso', coerced )
import Control.Lens.Operators
import Control.Lens.Operators.Minicute
import Control.Lens.Traversal ( partsOf )
import Control.Lens.Tuple
import Control.Monad.Extra ( concatMapM )
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

_dump :: Iso' Dump [DumpItem]
_dump = coerced
{-# INLINE _dump #-}


empty :: Dump
empty = Dump []
{-# INLINE empty #-}

saveState :: (MonadState s m, s ~ Dump) => DumpItem -> m ()
saveState di = _dump %= (di :)
{-# INLINE saveState #-}

loadState :: (MonadState s m, s ~ Dump, MonadFail m) => m DumpItem
loadState = _dump %%~= loadState'
  where
    loadState' (di : dis) = pure (di, dis)
    loadState' _ = fail "loadState: There is no dumped state to load"
    {-# INLINE loadState' #-}
{-# INLINE loadState #-}

extractAllAddresses :: (MonadState s m, s ~ Dump, MonadFail m) => m [Address]
extractAllAddresses = do
  addrStks <- use $ partsOf $ _dump . traverse . _2
  concatMapM (evalStateT AddressStack.peekAllAddrs) addrStks
{-# INLINE extractAllAddresses #-}


getAllDumpItems :: (MonadState s m, s ~ Dump) => m [DumpItem]
getAllDumpItems = use _dump
{-# INLINE getAllDumpItems #-}


emptyDumpItem :: DumpItem
emptyDumpItem
  = ( Code.empty
    , AddressStack.empty
    , ValueStack.empty
    )
{-# INLINE emptyDumpItem #-}
