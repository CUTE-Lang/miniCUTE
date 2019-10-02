{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Minicute.Data.GMachine.AddressStack
  ( module Minicute.Data.GMachine.Address

  , AddressStack
  , emptyAddressStack
  ) where

import Control.Lens.TH
import Data.Data
import GHC.Generics
import Minicute.Data.GMachine.Address

newtype AddressStack
  = AddressStack [Address]
  deriving ( Generic
           , Typeable
           , Data
           , Eq
           , Ord
           )

makeWrapped ''AddressStack

emptyAddressStack :: AddressStack
emptyAddressStack = AddressStack []
