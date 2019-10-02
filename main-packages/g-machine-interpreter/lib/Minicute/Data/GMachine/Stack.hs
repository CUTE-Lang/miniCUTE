{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Minicute.Data.GMachine.Stack
  ( module Minicute.Data.GMachine.Address

  , Stack
  , emptyStack
  ) where

import Control.Lens.TH
import Data.Data
import GHC.Generics
import Minicute.Data.GMachine.Address

newtype Stack
  = Stack [Address]
  deriving ( Generic
           , Typeable
           , Data
           , Eq
           , Ord
           )

makeWrapped ''Stack

emptyStack :: Stack
emptyStack = Stack []
