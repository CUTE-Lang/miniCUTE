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

import Control.Lens.Getter ( use )
import Control.Lens.Operators
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
popValue = do
  values <- use _Wrapped
  case values of
    value : values' -> do
      _Wrapped .= values'
      pure value
    _ -> fail "popValue: There is no value to pop"
