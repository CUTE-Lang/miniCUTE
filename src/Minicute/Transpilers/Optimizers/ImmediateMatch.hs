-- |
-- Optimizers to remove immediate matches.
module Minicute.Transpilers.Optimizers.ImmediateMatch
  ( immediateMatchMainL
  ) where

import Control.Lens.Each
import Control.Lens.Getter ( to )
import Control.Lens.Operators
import Control.Lens.Plated ( transformOf )
import Control.Lens.Wrapped ( _Wrapped )
import Data.Data.Lens ( uniplate )
import Data.List
import Minicute.Data.Minicute.Program

-- |
-- An optimizer to remove immediate matches in a whole program.
immediateMatchMainL :: MainProgramL -> MainProgramL
immediateMatchMainL = _Wrapped . each . _supercombinatorBody %~ immediateMatchMainEL

-- |
-- An optimizer to remove immediate matches in an expression.
immediateMatchMainEL :: MainExpressionL -> MainExpressionL
immediateMatchMainEL = transformOf uniplate go
  where
    go (ELLet flag lDefs (ELMatch (ELVariable v) mCases))
      | Just vLDef <- lookupLDefsL v lDefs
      , Just (tag, argExprs) <- destructDataExpression (vLDef ^. _letDefinitionBody)
      , Just vMCase <- lookupMCasesL tag mCases
      = let
          argBinders = vMCase ^. _matchCaseArguments
          matchLDefs = zipWith (curry LetDefinition) argBinders argExprs

          innerExpr = vMCase ^. _matchCaseBody
          expr
            = if not (null matchLDefs)
              then immediateMatchMainEL (ELLet NonRecursive matchLDefs innerExpr)
              else innerExpr
        in
          ELLet flag lDefs expr
    go e = e

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
lookupMCasesL :: Integer -> [MatchCase ExpressionL a] -> Maybe (MatchCase ExpressionL a)
lookupMCasesL tag = find (^. _matchCaseTag . to (== tag))

-- |
-- __TODO: move this into a Util or Data module__
lookupLDefsL :: (Eq a) => a -> [LetDefinition ExpressionL a] -> Maybe (LetDefinition ExpressionL a)
lookupLDefsL binder = find (^. _letDefinitionBinder . to (== binder))
