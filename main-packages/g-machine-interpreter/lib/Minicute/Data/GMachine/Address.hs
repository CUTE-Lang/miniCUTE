{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Minicute.Data.GMachine.Address
  ( GMachineAddress( .. )
  ) where

import Data.Data
import GHC.Generics

newtype GMachineAddress
  = GMachineAddress Integer
  deriving ( Generic
           , Typeable
           , Data
           , Eq
           , Num
           , Show
           )
