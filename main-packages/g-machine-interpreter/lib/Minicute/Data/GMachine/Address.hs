{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Minicute.Data.GMachine.Address
  ( GMachineAddress
  , gMachineAddress
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

-- |
-- Alias of 'fromInteger'
gMachineAddress :: Integer -> GMachineAddress
gMachineAddress = fromInteger
