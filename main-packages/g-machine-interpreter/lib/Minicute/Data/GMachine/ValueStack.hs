{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Copyright: (c) 2018-present Junyoung Clare Jang
-- License: BSD 3-Clause
module Minicute.Data.GMachine.ValueStack
  ( ValueStack
  , empty
  , pushValue
  , popValue

  , peekAllValues
  ) where

import Control.Lens.Getter ( use )
import Control.Lens.Iso ( Iso', coerced )
import Control.Lens.Operators
import Control.Lens.Operators.Minicute
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

_valueStack :: Iso' ValueStack [Integer]
_valueStack = coerced
{-# INLINE _valueStack #-}


empty :: ValueStack
empty = ValueStack []
{-# INLINE empty #-}

pushValue :: (MonadState s m, s ~ ValueStack) => Integer -> m ()
pushValue n = _valueStack %= (n :)
{-# INLINE pushValue #-}

popValue :: (MonadState s m, s ~ ValueStack, MonadFail m) => m Integer
popValue = _valueStack %%~= popValue'
  where
    popValue' (value : values) = pure (value, values)
    popValue' _ = fail "popValue: There is no value to pop"
    {-# INLINE popValue' #-}
{-# INLINE popValue #-}


peekAllValues :: (MonadState s m, s ~ ValueStack) => m [Integer]
peekAllValues = use _valueStack
{-# INLINE peekAllValues #-}
