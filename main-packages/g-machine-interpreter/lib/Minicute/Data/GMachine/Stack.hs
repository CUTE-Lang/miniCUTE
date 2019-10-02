{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Minicute.Data.GMachine.Stack
  ( module Minicute.Data.Common
  , module Minicute.Data.GMachine.Address

  , GMachineStack
  , emptyStack
  ) where

import Control.Lens.TH
import Data.Data
import GHC.Generics
import Minicute.Data.Common
import Minicute.Data.GMachine.Address

newtype GMachineStack
  = GMachineStack [GMachineAddress]
  deriving ( Generic
           , Typeable
           , Data
           , Eq
           , Ord
           )

makeWrapped ''GMachineStack

emptyStack :: GMachineStack
emptyStack = GMachineStack []
