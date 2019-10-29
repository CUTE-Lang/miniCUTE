{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Minicute.Data.GMachine.Address
  ( Address
  , address
  , minimumAddress
  , increaseAddress

  , prettyAddress
  ) where

import Data.Data
import GHC.Generics

import qualified Data.Text.Prettyprint.Doc as PP

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

prettyAddress :: Address -> PP.Doc ann
prettyAddress (Address addr) = PP.fuse PP.Shallow $ "&" PP.<> PP.pretty addr
