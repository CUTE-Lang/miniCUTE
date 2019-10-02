{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Minicute.Data.GMachine.ValueStack
  ( ValueStack
  , emptyValueStack
  ) where

import Control.Lens.TH
import Data.Data
import GHC.Generics

newtype ValueStack
  = ValueStack [Integer]
  deriving ( Generic
           , Typeable
           , Data
           , Eq
           , Ord
           )

makeWrapped ''ValueStack

emptyValueStack :: ValueStack
emptyValueStack = ValueStack []
