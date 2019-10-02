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
  , addAddressToGlobal
  , updateAddressInGlobal
  ) where

import Control.Lens.Operators
import Control.Lens.TH
import Control.Lens.Wrapped ( _Wrapped )
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
           )

makeWrapped ''Global

emptyGlobal :: Global
emptyGlobal = Global Map.empty

addAddressToGlobal :: (MonadState s m, s ~ Global) => Identifier -> Address -> m ()
addAddressToGlobal ident addr = _Wrapped %= Map.insert ident addr

updateAddressInGlobal :: (MonadState s m, s ~ Global) => Identifier -> Address -> m ()
updateAddressInGlobal ident addr = _Wrapped %= Map.insert ident addr
