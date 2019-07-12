{-# LANGUAGE GADTs #-}
-- |
-- Optimizers to remove immediate applications.
module Minicute.Transpilers.Optimizers.ImmediateApplication
  ( immediateApplicationMainL
  ) where

import Control.Lens.Each
import Control.Lens.Operators
import Control.Lens.Plated ( transformOf )
import Control.Lens.Wrapped ( _Wrapped )
import Data.Data.Lens ( uniplate )
import Minicute.Data.Minicute.Program

-- |
-- An optimizer to remove immediate applications in a whole program.
immediateApplicationMainL :: MainProgramL -> MainProgramL
immediateApplicationMainL = _Wrapped . each . _supercombinatorBody %~ immediateApplicationMainEL

-- |
-- An optimizer to remove immediate applications in an expression.
immediateApplicationMainEL :: MainExpressionL -> MainExpressionL
immediateApplicationMainEL = transformOf uniplate go
  where
    go (EApplication (ELambda (v : args') expr) e2)
      | not (null args') = ELambda args' expr'
      | otherwise = expr'
      where
        expr' = ELet NonRecursive [LetDefinition (v, e2)] expr
    go e = e
