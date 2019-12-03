{-# LANGUAGE DataKinds #-}
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

destructStructureExpression :: Expression 'Simple 'MC a -> Maybe (Integer, [Expression 'Simple 'MC a])
destructStructureExpression e = go e []
  where
    go (EConstructor _ tag arity) args
      | arity == genericLength args = Just (tag, args)
    go (EApplication _ e1 e2) args = go e1 (e2 : args)
    go _ _ = Nothing

lookupMCasesL :: Integer -> [MatchCase 'Simple 'MC a] -> Maybe (MatchCase 'Simple 'MC a)
lookupMCasesL tag = find (^. _matchCaseTag . to (== tag))

lookupLDefsL :: (Eq a) => a -> [LetDefinition 'Simple 'MC a] -> Maybe (LetDefinition 'Simple 'MC a)
lookupLDefsL binder = find (^. _letDefinitionBinder . to (== binder))
