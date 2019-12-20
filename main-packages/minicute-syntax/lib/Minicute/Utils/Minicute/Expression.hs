-- |
-- Copyright: (c) 2018-present Junyoung Clare Jang
-- License: BSD 3-Clause
--
-- Utilities for a miniCUTE expression
module Minicute.Utils.Minicute.Expression
  ( module Minicute.Data.Minicute.Expression

  , destructStructureExpression

  , lookupMCasesL
  , lookupLDefsL
  ) where

import Control.Lens.Getter ( to )
import Control.Lens.Operators
import Data.List
import Minicute.Data.Minicute.Expression

destructStructureExpression :: Expression t l a -> Maybe (Integer, [Expression t l a])
destructStructureExpression e = go e []
  where
    go (EConstructor _ tag arity) args
      | arity == genericLength args = Just (tag, args)
    go (EApplication _ e1 e2) args = go e1 (e2 : args)
    go _ _ = Nothing
{-# INLINABLE destructStructureExpression #-}

lookupMCasesL :: Integer -> [MatchCase t l a] -> Maybe (MatchCase t l a)
lookupMCasesL tag = find (^. _matchCaseTag . to (== tag))
{-# INLINABLE lookupMCasesL #-}

lookupLDefsL :: (Eq a) => a -> [LetDefinition t l a] -> Maybe (LetDefinition t l a)
lookupLDefsL binder = find (^. _letDefinitionBinder . to (== binder))
{-# INLINABLE lookupLDefsL #-}
