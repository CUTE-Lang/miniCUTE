{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Minicute.Data.GMachine.Address
  ( Address
  , address
  , minimumAddress
  , increaseAddress
  ) where

import Data.Data
import GHC.Generics

newtype Address
  = Address Integer
  deriving ( Generic
           , Typeable
           , Data
           , Eq
           , Ord
           , Show
           )

-- |
-- Constructor for 'Address'
address :: Integer -> Address
address = Address

minimumAddress :: Address
minimumAddress = Address 0

increaseAddress :: Address -> Address
increaseAddress (Address addr) = Address (addr + 1)
