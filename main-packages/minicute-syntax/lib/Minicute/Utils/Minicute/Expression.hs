{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
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

  , shallowDeclaredBinders
  , allDeclaredBinders

  , shallowUsedBinders
  , allUsedBinders
  ) where

import Control.Lens.Each ( each )
import Control.Lens.Getter ( to, view )
import Control.Lens.Lens ( ALens', cloneLens )
import Control.Lens.Operators
import Control.Lens.Plated ( cosmosOf )
import Data.Data ( Data )
import Data.Data.Lens ( uniplate )
import Data.List
import Minicute.Data.Minicute.Expression

import qualified Data.Set as Set

destructStructureExpression :: Expression t l a -> Maybe (Integer, [Expression t l a])
destructStructureExpression e = go e []
  where
    go (EConstructor _ tag arity) args
      | arity == genericLength args = Just (tag, args)
    go (EApplication _ e1 e2) args = go e1 (e2 : args)
    go _ _ = Nothing
{-# INLINE destructStructureExpression #-}

lookupMCasesL :: (Foldable m) => Integer -> m (MatchCase t l a) -> Maybe (MatchCase t l a)
lookupMCasesL tag = find (view $ _matchCaseTag . to (== tag))
{-# INLINE lookupMCasesL #-}

lookupLDefsL :: (Foldable m, Eq a) => a -> m (LetDefinition t l a) -> Maybe (LetDefinition t l a)
lookupLDefsL binder = find (view $ _letDefinitionBinder . to (== binder))
{-# INLINE lookupLDefsL #-}

shallowDeclaredBinders :: ALens' a Identifier -> Expression t l a -> Set.Set Identifier
shallowDeclaredBinders _ (EInteger _ _) = Set.empty
shallowDeclaredBinders _ (EConstructor _ _ _) = Set.empty
shallowDeclaredBinders _ (EPrimitive _ _) = Set.empty
shallowDeclaredBinders _ (EVariable _ _) = Set.empty
shallowDeclaredBinders _ (EApplication _ _ _) = Set.empty
shallowDeclaredBinders _a (ELet _ _ lDefs _) =
  Set.fromList $ lDefs ^.. each . _letDefinitionBinder . cloneLens _a
shallowDeclaredBinders _a (EMatch _ _ mCases) =
  Set.fromList $ mCases ^.. each . _matchCaseArguments . each . cloneLens _a
shallowDeclaredBinders _a (ELambda _ args _) =
  Set.fromList $ args ^.. each . cloneLens _a
{-# INLINE shallowDeclaredBinders #-}

allDeclaredBinders :: (Data (Expression t l a)) => ALens' a Identifier -> Expression t l a -> Set.Set Identifier
allDeclaredBinders _a = view $ cosmosOf uniplate . to (shallowDeclaredBinders _a)
{-# INLINE allDeclaredBinders #-}

shallowUsedBinders :: Expression t l a -> Set.Set Identifier
shallowUsedBinders (EInteger _ _) = Set.empty
shallowUsedBinders (EConstructor _ _ _) = Set.empty
shallowUsedBinders (EPrimitive _ _) = Set.empty
shallowUsedBinders (EVariable _ v) = Set.singleton v
shallowUsedBinders (EApplication _ _ _) = Set.empty
shallowUsedBinders (ELet _ _ _ _) = Set.empty
shallowUsedBinders (EMatch _ _ _) = Set.empty
shallowUsedBinders (ELambda _ _ _) = Set.empty
{-# INLINE shallowUsedBinders #-}

allUsedBinders :: (Data (Expression t l a)) => Expression t l a -> Set.Set Identifier
allUsedBinders = view $ cosmosOf uniplate . to shallowUsedBinders
{-# INLINE allUsedBinders #-}
