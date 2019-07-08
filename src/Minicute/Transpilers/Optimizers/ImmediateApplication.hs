-- |
-- Optimizers to remove immediate applications.
module Minicute.Transpilers.Optimizers.ImmediateApplication
  ( immediateApplicationMainL
  ) where

import Control.Lens.Each
import Control.Lens.Operators
import Control.Lens.Wrapped ( _Wrapped )
import Minicute.Data.Minicute.Program

-- |
-- An optimizer to remove immediate applications in a whole program.
immediateApplicationMainL :: MainProgramL -> MainProgramL
immediateApplicationMainL = _Wrapped . each . _supercombinatorBody %~ immediateApplicationMainEL

-- |
-- An optimizer to remove immediate applications in an expression.
--
-- __TODO: use uniplate to remove boilerplate__
immediateApplicationMainEL :: MainExpressionL -> MainExpressionL
immediateApplicationMainEL e@(ELInteger _) = e
immediateApplicationMainEL e@(ELConstructor _ _) = e
immediateApplicationMainEL e@(ELVariable _) = e
immediateApplicationMainEL (ELApplication (ELLambda (v : args') expr) e2)
  | not (null args')
  = ELLambda args' expr'
  | otherwise
  = expr'
  where
    expr' = ELLet NonRecursive [LetDefinitionL v e2] (immediateApplicationMainEL expr)
-- uniplate can make the following case simple.
-- (Bottom-up way can make the case simple.)
immediateApplicationMainEL (ELApplication e1 e2)
  = case e1' of
      ELLambda _ _ -> immediateApplicationMainEL e'
      _ -> e'
  where
    e' = ELApplication e1' e2'

    e1' = immediateApplicationMainEL e1
    e2' = immediateApplicationMainEL e2
immediateApplicationMainEL (ELLet flag lDefs expr)
  = ELLet flag (lDefs & each . _letDefinitionBody %~ immediateApplicationMainEL) (immediateApplicationMainEL expr)
immediateApplicationMainEL (ELMatch expr mCases)
  = ELMatch (immediateApplicationMainEL expr) (mCases & each . _matchCaseBody %~ immediateApplicationMainEL)
immediateApplicationMainEL (ELLambda args expr)
  = ELLambda args (immediateApplicationMainEL expr)
