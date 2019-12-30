{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Copyright: (c) 2018-present Junyoung Clare Jang
-- License: BSD 3-Clause
module Minicute.Data.GMachine.Address
  ( Address
  , address
  , minimumAddress
  , increaseAddress
  ) where

import Control.Lens.Iso ( coerced )
import Control.Lens.Setter ( over )
import Control.Lens.Type
import Data.Data ( Data, Typeable )
import Data.Text.Prettyprint.Doc ( Pretty(..) )
import GHC.Generics ( Generic )

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
{-# INLINE address #-}

minimumAddress :: Address
minimumAddress = address 0
{-# INLINE minimumAddress #-}

increaseAddress :: Address -> Address
increaseAddress = over (coerced :: Iso' Address Integer) (+ 1)
{-# INLINE increaseAddress #-}

instance Pretty Address where
  pretty (Address addr) = PP.fuse PP.Shallow $ "&" <> pretty addr
  {-# INLINE pretty #-}
