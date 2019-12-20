{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
-- |
-- Copyright: (c) 2018-present Junyoung Clare Jang
-- License: BSD 3-Clause
--
-- Optimizers to merge consecutive lambda expressions.
module Minicute.Transpilers.Optimizers.LambdaMerge
  ( module Minicute.Data.Minicute.Program

  , lambdaMergeMainMC
  ) where

import Control.Lens.Each
import Control.Lens.Operators
import Control.Lens.Plated ( transformOf )
import Control.Lens.Wrapped ( _Wrapped )
import Data.Data.Lens ( uniplate )
import Minicute.Data.Minicute.Program

-- |
-- An optimizer to merge consecutive lambda expressions in a whole program.
lambdaMergeMainMC :: MainProgram 'Simple 'MC -> MainProgram 'Simple 'MC
lambdaMergeMainMC
  = _Wrapped . each . _supercombinatorBody %~ lambdaMergeMainEMC
{-# INLINABLE lambdaMergeMainMC #-}

-- |
-- An optimizer to merge consecutive lambda expressions in an expression.
lambdaMergeMainEMC :: MainExpression 'Simple 'MC -> MainExpression 'Simple 'MC
lambdaMergeMainEMC = transformOf uniplate go
  where
    go :: MainExpression 'Simple 'MC -> MainExpression 'Simple 'MC
    go (ELambda _ args0 (ELambda _ args1 expr))
      = ELambda () (args0 <> args1) expr
    go e = e
{-# INLINABLE lambdaMergeMainEMC #-}
