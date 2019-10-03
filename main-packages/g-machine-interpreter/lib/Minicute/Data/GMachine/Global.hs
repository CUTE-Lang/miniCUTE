{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Minicute.Data.GMachine.Global
  ( module Minicute.Data.Common
  , module Minicute.Data.GMachine.Address

  , Global
  , emptyGlobal
  , allocAddress
  , updateAddress
  , findAddress
  ) where

import Control.Lens.At ( at )
import Control.Lens.Getter ( use )
import Control.Lens.Operators
import Control.Lens.TH
import Control.Lens.Wrapped ( _Wrapped )
import Control.Monad.Fail ( MonadFail )
import Control.Monad.State
import Data.Data
import GHC.Generics
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

emptyGlobal :: Global
emptyGlobal = Global Map.empty

allocAddress :: (MonadState s m, s ~ Global) => Identifier -> Address -> m ()
allocAddress ident addr = _Wrapped %= Map.insert ident addr

updateAddress :: (MonadState s m, s ~ Global) => Identifier -> Address -> m ()
updateAddress ident addr = _Wrapped %= Map.insert ident addr

findAddress :: (MonadState s m, s ~ Global, MonadFail m) => Identifier -> m Address
findAddress ident = do
  mayAddress <- use (_Wrapped . at ident)
  case mayAddress of
    Just addr -> return addr
    Nothing -> fail ("findAddress: No registered address for the identifier " <> show ident)
