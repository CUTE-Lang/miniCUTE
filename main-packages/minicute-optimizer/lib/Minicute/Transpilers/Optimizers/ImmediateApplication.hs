{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
-- |
-- Copyright: (c) 2018-present Junyoung Clare Jang
-- License: BSD 3-Clause
--
-- Optimizers to remove immediate applications.
module Minicute.Transpilers.Optimizers.ImmediateApplication
  ( module Minicute.Data.Minicute.Program

  , immediateApplicationMainMC
  ) where

import Control.Lens.Each
import Control.Lens.Operators
import Control.Lens.Plated ( transformOf )
import Control.Lens.Wrapped ( _Wrapped )
import Data.Data.Lens ( uniplate )
import Minicute.Data.Minicute.Program

-- |
-- An optimizer to remove immediate applications in a whole program.
immediateApplicationMainMC :: MainProgram 'Simple 'MC -> MainProgram 'Simple 'MC
immediateApplicationMainMC
  = _Wrapped . each . _supercombinatorBody %~ immediateApplicationMainEMC
{-# INLINABLE immediateApplicationMainMC #-}

-- |
-- An optimizer to remove immediate applications in an expression.
immediateApplicationMainEMC :: MainExpression 'Simple 'MC -> MainExpression 'Simple 'MC
immediateApplicationMainEMC = transformOf uniplate go
  where
    go :: MainExpression 'Simple 'MC -> MainExpression 'Simple 'MC
    go (EApplication _ (ELambda _ (v : args') expr) e2)
      = case args' of
          _ : _ -> ELambda () args' expr'
          _ -> expr'
      where
        expr' = ELet () NonRecursive [LetDefinition (v, e2)] expr
    go e = e
{-# INLINABLE immediateApplicationMainEMC #-}
