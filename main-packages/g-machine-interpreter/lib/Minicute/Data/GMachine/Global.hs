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

  , GMachineGlobal
  , emptyGlobal
  , addAddressToGlobal
  , updateAddressInGlobal
  ) where

import Control.Lens.Operators
import Control.Lens.TH
import Control.Lens.Wrapped ( _Wrapped )
import Control.Monad.Fail ( MonadFail )
import Control.Monad.State
import Data.Data
import GHC.Generics
import Minicute.Data.Common
import Minicute.Data.GMachine.Address

import qualified Data.Map as Map

newtype GMachineGlobal
  = GMachineGlobal (Map.Map Identifier GMachineAddress)
  deriving ( Generic
           , Typeable
           , Data
           , Eq
           )

makeWrapped ''GMachineGlobal

emptyGlobal :: GMachineGlobal
emptyGlobal = GMachineGlobal Map.empty

addAddressToGlobal :: (MonadState s m, s ~ GMachineGlobal, MonadFail m) => Identifier -> GMachineAddress -> m ()
addAddressToGlobal ident addr
  = _Wrapped %= Map.insert ident addr

updateAddressInGlobal :: Identifier -> GMachineAddress -> GMachineGlobal -> GMachineGlobal
updateAddressInGlobal ident addr = _Wrapped %~ Map.insert ident addr
