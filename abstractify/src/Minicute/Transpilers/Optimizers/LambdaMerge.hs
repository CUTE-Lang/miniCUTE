{-# LANGUAGE GADTs #-}
-- |
-- Optimizers to merge consecutive lambda expressions.
module Minicute.Transpilers.Optimizers.LambdaMerge
  ( lambdaMergeMainMC
  ) where

import Control.Lens.Each
import Control.Lens.Operators
import Control.Lens.Plated ( transformOf )
import Control.Lens.Wrapped ( _Wrapped )
import Data.Data.Lens ( uniplate )
import Minicute.Data.Minicute.Program

-- |
-- An optimizer to merge consecutive lambda expressions in a whole program.
lambdaMergeMainMC :: MainProgramMC -> MainProgramMC
lambdaMergeMainMC = _Wrapped . each . _supercombinatorBody %~ lambdaMergeMainEMC

-- |
-- An optimizer to merge consecutive lambda expressions in an expression.
lambdaMergeMainEMC :: MainExpressionMC -> MainExpressionMC
lambdaMergeMainEMC = transformOf uniplate go
  where
    go (ELambda args0 (ELambda args1 expr)) = ELambda (args0 <> args1) expr
    go e = e
