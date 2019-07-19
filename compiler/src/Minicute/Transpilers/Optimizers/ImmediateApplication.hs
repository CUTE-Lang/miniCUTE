{-# LANGUAGE GADTs #-}
-- |
-- Optimizers to remove immediate applications.
module Minicute.Transpilers.Optimizers.ImmediateApplication
  ( immediateApplicationMainMC
  ) where

import Control.Lens.Each
import Control.Lens.Operators
import Control.Lens.Plated ( transformOf )
import Control.Lens.Wrapped ( _Wrapped )
import Data.Data.Lens ( uniplate )
import Minicute.Data.Minicute.Program

-- |
-- An optimizer to remove immediate applications in a whole program.
immediateApplicationMainMC :: MainProgramMC -> MainProgramMC
immediateApplicationMainMC = _Wrapped . each . _supercombinatorBody %~ immediateApplicationMainEMC

-- |
-- An optimizer to remove immediate applications in an expression.
immediateApplicationMainEMC :: MainExpressionMC -> MainExpressionMC
immediateApplicationMainEMC = transformOf uniplate go
  where
    go (EApplication (ELambda (v : args') expr) e2)
      | not (null args') = ELambda args' expr'
      | otherwise = expr'
      where
        expr' = ELet NonRecursive [LetDefinition (v, e2)] expr
    go e = e
