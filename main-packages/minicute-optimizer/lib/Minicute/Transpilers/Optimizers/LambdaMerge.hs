{-# LANGUAGE DataKinds #-}
-- |
-- Copyright: (c) 2018-present Junyoung Clare Jang
-- License: BSD 3-Clause
--
-- Optimizers to merge consecutive lambda expressions.
module Minicute.Transpilers.Optimizers.LambdaMerge
  ( module Minicute.Data.Minicute.Program

  , lambdaMergeMC
  ) where

import Control.Lens.Each
import Control.Lens.Operators
import Control.Lens.Plated ( transformOf )
import Control.Lens.Wrapped ( _Wrapped )
import Data.Data ( Data )
import Data.Data.Lens ( uniplate )
import Minicute.Data.Minicute.Program

-- |
-- An optimizer to merge consecutive lambda expressions in a whole program.
lambdaMergeMC :: (Data a) => Program 'Simple 'MC a -> Program 'Simple 'MC a
lambdaMergeMC
  = _Wrapped . each . _supercombinatorBody %~ lambdaMergeEMC
{-# INLINE lambdaMergeMC #-}

-- |
-- An optimizer to merge consecutive lambda expressions in an expression.
lambdaMergeEMC :: (Data a) => Expression 'Simple 'MC a -> Expression 'Simple 'MC a
lambdaMergeEMC = transformOf uniplate go
  where
    go :: (Data a) => Expression 'Simple 'MC a -> Expression 'Simple 'MC a
    go (ELambda _ args0 (ELambda _ args1 expr))
      = ELambda () (args0 <> args1) expr
    go e = e
{-# INLINE lambdaMergeEMC #-}
