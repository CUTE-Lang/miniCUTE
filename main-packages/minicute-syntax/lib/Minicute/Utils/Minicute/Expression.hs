-- |
-- Copyright: (c) 2018-present Junyoung Clare Jang
-- License: BSD 3-Clause
--
-- Utilities for a miniCUTE expression
module Minicute.Utils.Minicute.Expression
  ( destructStructureExpression

  , lookupMCasesL
  , lookupLDefsL
  ) where

import Control.Lens.Getter ( to )
import Control.Lens.Operators
import Data.List
import Minicute.Data.Minicute.Expression

destructStructureExpression :: ExpressionMC a -> Maybe (Integer, [ExpressionMC a])
destructStructureExpression e = go e []
  where
    go (EConstructor tag arity) args
      | arity == genericLength args = Just (tag, args)
    go (EApplication e1 e2) args = go e1 (e2 : args)
    go _ _ = Nothing

lookupMCasesL :: Integer -> [MatchCase ExpressionMC a] -> Maybe (MatchCase ExpressionMC a)
lookupMCasesL tag = find (^. _matchCaseTag . to (== tag))

lookupLDefsL :: (Eq a) => a -> [LetDefinition ExpressionMC a] -> Maybe (LetDefinition ExpressionMC a)
lookupLDefsL binder = find (^. _letDefinitionBinder . to (== binder))
