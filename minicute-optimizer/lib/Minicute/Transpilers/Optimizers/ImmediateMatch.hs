-- |
-- Optimizers to remove immediate matches.
module Minicute.Transpilers.Optimizers.ImmediateMatch
  ( immediateMatchMainMC
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
immediateMatchMainMC :: MainProgramMC -> MainProgramMC
immediateMatchMainMC = _Wrapped . each . _supercombinatorBody %~ immediateMatchMainEMC

-- |
-- An optimizer to remove immediate matches in an expression.
immediateMatchMainEMC :: MainExpressionMC -> MainExpressionMC
immediateMatchMainEMC = transformOf uniplate go
  where
    go (ELet flag lDefs (EMatch (EVariable v) mCases))
      | Just vLDef <- lookupLDefsL v lDefs
      , Just (tag, argExprs) <- destructDataExpression (vLDef ^. _letDefinitionBody)
      , Just vMCase <- lookupMCasesL tag mCases
      = let
          argBinders = vMCase ^. _matchCaseArguments
          matchLDefs = zipWith (curry LetDefinition) argBinders argExprs

          innerExpr = vMCase ^. _matchCaseBody
          expr
            = if not (null matchLDefs)
              then immediateMatchMainEMC (ELet NonRecursive matchLDefs innerExpr)
              else innerExpr
        in
          ELet flag lDefs expr
    go e = e

-- |
-- __TODO: move this into a Util or Data module__
destructDataExpression :: ExpressionMC a -> Maybe (Integer, [ExpressionMC a])
destructDataExpression e = go e []
  where
    go (EConstructor tag arity) args
      | arity == genericLength args = Just (tag, args)
    go (EApplication e1 e2) args = go e1 (e2 : args)
    go _ _ = Nothing

-- |
-- __TODO: move this into a Util or Data module__
lookupMCasesL :: Integer -> [MatchCase ExpressionMC a] -> Maybe (MatchCase ExpressionMC a)
lookupMCasesL tag = find (^. _matchCaseTag . to (== tag))

-- |
-- __TODO: move this into a Util or Data module__
lookupLDefsL :: (Eq a) => a -> [LetDefinition ExpressionMC a] -> Maybe (LetDefinition ExpressionMC a)
lookupLDefsL binder = find (^. _letDefinitionBinder . to (== binder))
