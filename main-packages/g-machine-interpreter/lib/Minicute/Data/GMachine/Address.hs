{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Minicute.Data.GMachine.Address
  ( Address
  , address
  , minimumAddress
  , increaseAddress
  ) where

import Data.Data
import Data.Text.Prettyprint.Doc ( Pretty(..) )
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

instance Pretty Address where
  pretty (Address addr) = PP.fuse PP.Shallow $ "&" PP.<> pretty addr

-- |
-- Constructor for 'Address'
address :: Integer -> Address
address = Address

minimumAddress :: Address
minimumAddress = Address 0

increaseAddress :: Address -> Address
increaseAddress (Address addr) = Address (addr + 1)
