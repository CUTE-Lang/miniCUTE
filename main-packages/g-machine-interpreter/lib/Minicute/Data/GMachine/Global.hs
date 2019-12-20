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
module Minicute.Data.GMachine.Global
  ( module Minicute.Data.Common
  , module Minicute.Data.GMachine.Address

  , Global
  , empty
  , allocAddress
  , updateAddress
  , findAddress
  , findAllAddresses
  ) where

import Prelude hiding ( fail )

import Control.Lens.At ( at )
import Control.Lens.Getter ( use )
import Control.Lens.Operators
import Control.Lens.Operators.Minicute
import Control.Lens.TH
import Control.Lens.Traversal ( partsOf )
import Control.Lens.Wrapped ( _Wrapped )
import Control.Monad.Fail
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

makeWrapped ''Global

empty :: Global
empty = Global Map.empty
{-# INLINE empty #-}

allocAddress :: (MonadState s m, s ~ Global) => Identifier -> Address -> m ()
allocAddress ident addr = _Wrapped . at ident .= Just addr
{-# INLINABLE allocAddress #-}

updateAddress :: (MonadState s m, s ~ Global, MonadFail m) => Identifier -> Address -> m ()
updateAddress ident addr = _Wrapped . at ident %%~= updateAddress'
  where
    updateAddress' (Just _) = pure ((), Just addr)
    updateAddress' Nothing = fail $ "updateAddress: No registered address for the identifier " <> show ident
    {-# INLINABLE updateAddress' #-}
{-# INLINABLE updateAddress #-}

findAddress :: (MonadState s m, s ~ Global, MonadFail m) => Identifier -> m Address
findAddress ident = use (_Wrapped . at ident) >>= findAddress'
  where
    findAddress' (Just addr) = pure addr
    findAddress' Nothing = fail $ "findAddress: No registered address for the identifier " <> show ident
    {-# INLINABLE findAddress' #-}
{-# INLINABLE findAddress #-}

findAllAddresses :: (MonadState s m, s ~ Global, MonadFail m) => m [Address]
findAllAddresses = use (_Wrapped . partsOf traverse)
{-# INLINABLE findAllAddresses #-}
