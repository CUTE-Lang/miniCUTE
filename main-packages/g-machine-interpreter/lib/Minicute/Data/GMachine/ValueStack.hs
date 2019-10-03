{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Minicute.Data.GMachine.ValueStack
  ( ValueStack
  , empty
  , pushValue
  , popValue
  ) where

import Prelude hiding ( fail )

import Control.Lens.Operators
import Control.Lens.Operators.Minicute
import Control.Lens.TH
import Control.Lens.Wrapped ( _Wrapped )
import Control.Monad.Fail
import Control.Monad.State ( MonadState )
import Data.Data ( Data, Typeable )
import GHC.Generics ( Generic )

newtype ValueStack
  = ValueStack [Integer]
  deriving ( Generic
           , Typeable
           , Data
           , Eq
           , Ord
           , Show
           )

makeWrapped ''ValueStack

empty :: ValueStack
empty = ValueStack []

pushValue :: (MonadState s m, s ~ ValueStack) => Integer -> m ()
pushValue n = _Wrapped %= (n :)

popValue :: (MonadState s m, s ~ ValueStack, MonadFail m) => m Integer
popValue = _Wrapped %%~= popValue'
  where
    popValue' (value : values) = pure (value, values)
    popValue' _ = fail "popValue: There is no value to pop"
