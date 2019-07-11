-- |
-- Optimizers to reduce simple arithmetic expressions.
module Minicute.Transpilers.Optimizers.SimpleArithmetic
  ( simpleArithmeticMainL
  ) where

import Control.Lens.Each
import Control.Lens.Operators
import Control.Lens.Plated ( transformOf )
import Control.Lens.Wrapped ( _Wrapped )
import Data.Data.Lens ( uniplate )
import Minicute.Data.Minicute.Program

-- |
-- An optimizer to reduce simple arithmetic expressions in a whole program.
simpleArithmeticMainL :: MainProgramL -> MainProgramL
simpleArithmeticMainL = _Wrapped . each . _supercombinatorBody %~ simpleArithmeticMainEL

-- |
-- An optimizer to reduce simple arithmetic expressions in an expression.
simpleArithmeticMainEL :: MainExpressionL -> MainExpressionL
simpleArithmeticMainEL = transformOf uniplate go
  where
    -- __TODO: replace hard-corded op checking__
    go (ELApplication2 (ELVariable (Identifier op)) (ELInteger n1) (ELInteger n2))
      | op == "+" = ELInteger (n1 + n2)
      | op == "-" = ELInteger (n1 - n2)
      | op == "*" = ELInteger (n1 * n2)
      -- "/" is not optimized yet... It needs a floating point support
      --
      -- __TODO: Add "/" optimization__
    go e = e
