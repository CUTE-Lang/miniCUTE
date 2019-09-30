{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Minicute.Data.GMachine.Address
  ( GMachineAddress( .. )
  ) where

import Data.Data
import GHC.Generics
import Language.Haskell.TH.Syntax

newtype GMachineAddress
  = GMachineAddress Integer
  deriving ( Generic
           , Typeable
           , Data
           , Lift
           , Eq
           , Ord
           , Num
           , Show
           )
