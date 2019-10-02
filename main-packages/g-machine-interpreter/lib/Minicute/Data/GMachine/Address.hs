{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Minicute.Data.GMachine.Address
  ( Address
  , address
  ) where

import Data.Data
import GHC.Generics

newtype Address
  = Address Integer
  deriving ( Generic
           , Typeable
           , Data
           , Eq
           , Num
           , Show
           )

-- |
-- Alias of 'fromInteger'
address :: Integer -> Address
address = fromInteger
