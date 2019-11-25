-- |
-- Copyright: (c) 2018-present Junyoung Clare Jang
-- License: BSD 3-Clause
--
-- Optimizers to reduce simple arithmetic expressions.
module Minicute.Transpilers.Optimizers.SimpleArithmetic
  ( simpleArithmeticMainMC
  ) where

import Control.Lens.Each
import Control.Lens.Operators
import Control.Lens.Plated ( transformOf )
import Control.Lens.Wrapped ( _Wrapped )
import Data.Data.Lens ( uniplate )
import Minicute.Data.Minicute.Program

-- |
-- An optimizer to reduce simple arithmetic expressions in a whole program.
simpleArithmeticMainMC :: MainProgramMC -> MainProgramMC
simpleArithmeticMainMC = _Wrapped . each . _supercombinatorBody %~ simpleArithmeticMainEMC

-- |
-- An optimizer to reduce simple arithmetic expressions in an expression.
simpleArithmeticMainEMC :: MainExpressionMC -> MainExpressionMC
simpleArithmeticMainEMC = transformOf uniplate go
  where
    -- __TODO: replace hard-corded op checking__
    go e@(EApplication2 (EPrimitive prim) (EInteger n1) (EInteger n2))
      = case prim of
          PrimAdd -> EInteger (n1 + n2)
          PrimSub -> EInteger (n1 - n2)
          PrimMul -> EInteger (n1 * n2)
          _ -> e
      -- 'PrimDiv' is not optimized yet... It needs a floating point support
      --
      -- __TODO: Add 'PrimDiv' optimization__
    go e = e
