-- |
-- Optimizers to remove immediate match.
module Minicute.Transpilers.Optimizers.ImmediateMatch
  ( immediateMatchMainL
  ) where

import Control.Lens.Each
import Control.Lens.Getter ( to )
import Control.Lens.Operators
import Control.Lens.Wrapped ( _Wrapped )
import Data.List
import Minicute.Data.Minicute.Program

-- |
-- An optimizer to remove immediate match in a whole program.
immediateMatchMainL :: MainProgramL -> MainProgramL
immediateMatchMainL = _Wrapped . each . _supercombinatorBody %~ immediateMatchMainEL

-- |
-- An optimizer to remove immediate match in an expression.
--
-- __TODO: use uniplate to remove boilerplate__
immediateMatchMainEL :: MainExpressionL -> MainExpressionL
immediateMatchMainEL e@(ELInteger _) = e
immediateMatchMainEL e@(ELConstructor _ _) = e
immediateMatchMainEL e@(ELVariable _) = e
immediateMatchMainEL (ELApplication e1 e2)
  = ELApplication (immediateMatchMainEL e1) (immediateMatchMainEL e2)
immediateMatchMainEL (ELLet flag lDefs (ELMatch (ELVariable v) mCases))
  | Just vLDef <- lookupLDefsL v lDefs
  , Just (tag, argExprs) <- destructDataExpression (vLDef ^. _letDefinitionBody)
  , Just vMCase <- lookupMCasesL tag mCases
  = let
      argBinders = vMCase ^. _matchCaseArguments
      matchLDefs = zipWith LetDefinitionL argBinders argExprs

      innerExpr = immediateMatchMainEL (vMCase ^. _matchCaseBody)
      expr
        = if not (null matchLDefs)
          then ELLet NonRecursive matchLDefs innerExpr
          else innerExpr
    in
      ELLet flag lDefs expr
immediateMatchMainEL (ELLet flag lDefs expr)
  = ELLet flag (lDefs & each . _letDefinitionBody %~ immediateMatchMainEL) (immediateMatchMainEL expr)
immediateMatchMainEL (ELMatch expr mCases)
  = ELMatch (immediateMatchMainEL expr) (mCases & each . _matchCaseBody %~ immediateMatchMainEL)
immediateMatchMainEL (ELLambda args expr)
  = ELLambda args (immediateMatchMainEL expr)

-- |
-- __TODO: move this into a Util or Data module__
destructDataExpression :: ExpressionL a -> Maybe (Integer, [ExpressionL a])
destructDataExpression e = go e []
  where
    go (ELConstructor tag arity) args
      | arity == genericLength args = Just (tag, args)
    go (ELApplication e1 e2) args = go e1 (e2 : args)
    go _ _ = Nothing

-- |
-- __TODO: move this into a Util or Data module__
lookupMCasesL :: Integer -> [MatchCaseL a] -> Maybe (MatchCaseL a)
lookupMCasesL tag = find (^. _matchCaseTag . to (== tag))

-- |
-- __TODO: move this into a Util or Data module__
lookupLDefsL :: (Eq a) => a -> [LetDefinitionL a] -> Maybe (LetDefinitionL a)
lookupLDefsL binder = find (^. _letDefinitionBinder . to (== binder))
