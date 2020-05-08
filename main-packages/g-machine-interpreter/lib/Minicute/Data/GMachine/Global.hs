{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Copyright: (c) 2018-present Junyoung Clare Jang
-- License: BSD 3-Clause
module Minicute.Data.GMachine.Global
  ( module Minicute.Data.Common
  , module Minicute.Data.GMachine.Address

  , Global
  , empty
  , allocAddress
  , updateAddress
  , findAddress
  , findAllAddresses

  , getGlobalMap
  ) where

import Control.Lens.At ( at )
import Control.Lens.Getter ( use )
import Control.Lens.Iso ( Iso', coerced )
import Control.Lens.Operators
import Control.Lens.Operators.Minicute
import Control.Lens.Traversal ( partsOf )
import Control.Monad.State ( MonadState )
import Data.Data ( Data, Typeable )
import GHC.Generics ( Generic )
import Minicute.Data.Common ( Identifier(..) )
import Minicute.Data.GMachine.Address

import qualified Data.Map as Map

newtype Global
  = Global (Map.Map Identifier Address)
  deriving ( Generic
           , Typeable
           , Data
           , Eq
           , Ord
           , Show
           )

_global :: Iso' Global (Map.Map Identifier Address)
_global = coerced
{-# INLINE _global #-}


empty :: Global
empty = Global Map.empty
{-# INLINE empty #-}

allocAddress :: (MonadState s m, s ~ Global) => Identifier -> Address -> m ()
allocAddress ident addr = _global . at ident .= Just addr
{-# INLINE allocAddress #-}

updateAddress :: (MonadState s m, s ~ Global, MonadFail m) => Identifier -> Address -> m ()
updateAddress ident addr = _global . at ident %%~= updateAddress'
  where
    updateAddress' (Just _) = pure ((), Just addr)
    updateAddress' Nothing = fail $ "updateAddress: No registered address for the identifier " <> show ident
    {-# INLINE updateAddress' #-}
{-# INLINE updateAddress #-}

findAddress :: (MonadState s m, s ~ Global, MonadFail m) => Identifier -> m Address
findAddress ident = use (_global . at ident) >>= findAddress'
  where
    findAddress' (Just addr) = pure addr
    findAddress' Nothing = fail $ "findAddress: No registered address for the identifier " <> show ident
    {-# INLINE findAddress' #-}
{-# INLINE findAddress #-}

findAllAddresses :: (MonadState s m, s ~ Global, MonadFail m) => m [Address]
findAllAddresses = use (_global . partsOf traverse)
{-# INLINE findAllAddresses #-}


getGlobalMap :: (MonadState s m, s ~ Global) => m (Map.Map Identifier Address)
getGlobalMap = use _global
{-# INLINE getGlobalMap #-}
