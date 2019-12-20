-- |
-- Copyright: (c) 2018-present Junyoung Clare Jang
-- License: BSD 3-Clause
module Minicute.Test.Common.Gen
  ( integer
  , identifier
  ) where

import Prelude hiding ( fail )

import Hedgehog

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

integer :: (MonadGen m) => m Integer
integer = Gen.integral (Range.constant 0 1000000000000)
{-# INLINABLE integer #-}

identifier :: (MonadGen m) => m String
identifier = (:) <$> Gen.choice [Gen.alpha, Gen.constant '_'] <*> Gen.string (Range.linear 0 100) (Gen.choice [Gen.alphaNum, Gen.constant '_'])
{-# INLINABLE identifier #-}
