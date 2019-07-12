{-# LANGUAGE DataKinds #-}
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
              then immediateMatchMainEL (ELet NonRecursive matchLDefs innerExpr)
              else innerExpr
        in
          ELet flag lDefs expr
    go e = e

-- |
-- __TODO: move this into a Util or Data module__
destructDataExpression :: Expression 'MC a -> Maybe (Integer, [Expression 'MC a])
destructDataExpression e = go e []
  where
    go (EConstructor tag arity) args
      | arity == genericLength args = Just (tag, args)
    go (EApplication e1 e2) args = go e1 (e2 : args)
    go _ _ = Nothing

-- |
-- __TODO: move this into a Util or Data module__
lookupMCasesL :: Integer -> [MatchCase (Expression 'MC) a] -> Maybe (MatchCase (Expression 'MC) a)
lookupMCasesL tag = find (^. _matchCaseTag . to (== tag))

-- |
-- __TODO: move this into a Util or Data module__
lookupLDefsL :: (Eq a) => a -> [LetDefinition (Expression 'MC) a] -> Maybe (LetDefinition (Expression 'MC) a)
lookupLDefsL binder = find (^. _letDefinitionBinder . to (== binder))
