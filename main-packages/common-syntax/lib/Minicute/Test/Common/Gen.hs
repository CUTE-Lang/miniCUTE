-- |
-- Copyright: (c) 2018-present Junyoung Clare Jang
-- License: BSD 3-Clause
module Minicute.Test.Common.Gen
  ( integer
  , identifierString
  , identifier
  , primitive
  ) where

import Prelude hiding ( fail )

import Hedgehog
import Minicute.Data.Common

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

integer :: (MonadGen m) => m Integer
integer = Gen.integral (Range.constant 0 1000000000000)
{-# INLINABLE integer #-}

identifierString :: (MonadGen m) => m String
identifierString
  = (:)
    <$> Gen.choice [Gen.alpha, Gen.constant '_']
    <*> Gen.string (Range.linear 0 100) (Gen.choice [Gen.alphaNum, Gen.constant '_'])
{-# INLINABLE identifierString #-}

identifier :: (MonadGen m) => m Identifier
identifier = Identifier <$> identifierString
{-# INLINABLE identifier #-}

primitive :: (MonadGen m) => m Primitive
primitive = Gen.element $ fst <$> primitivePrecedenceTable
{-# INLINABLE primitive #-}
