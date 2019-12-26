{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
-- |
-- Copyright: (c) 2018-present Junyoung Clare Jang
-- License: BSD 3-Clause
module Minicute.Test.Minicute.Gen
  ( mainExpression
  , expression
  ) where

import Prelude hiding ( fail )

import Control.Lens.Each ( each )
import Control.Lens.Lens ( ALens', cloneLens )
import Control.Lens.Operators
import Control.Monad.Reader ( MonadReader(..), ReaderT(..) )
import Control.Monad.Trans ( lift )
import Hedgehog
import Minicute.Data.Minicute.Program

import qualified Data.Set as Set
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Minicute.Test.Common.Gen as Gen

mainExpression :: (MonadGen m, GenExpression l) => m (Annotation t) -> m (Expression t l Identifier)
mainExpression = expression id Gen.identifier

class GenExpression (l :: ExpressionLevel) where
  expression :: (MonadGen m, Ord a) => ALens' a Identifier -> m a -> m (Annotation t) -> m (Expression t l a)

instance GenExpression 'MC where
  expression _a (lift -> genBinder) (lift -> genAnn)
    = runReaderT expression' Set.empty
    where
      expression'
        = Gen.recursive
          Gen.choice
          [ integerExpression genAnn
          , constructorExpression genAnn
          , primitiveExpression genAnn
          , variableExpression genAnn
          ]
          [ applicationExpression genAnn expression'
          , letExpression _a genBinder genAnn expression'
          , matchExpression _a genBinder genAnn expression'
          , lambdaExpression _a genBinder genAnn expression'
          ]
      {-# INLINABLE expression' #-}
  {-# INLINE expression #-}

instance GenExpression 'LLMC where
  expression _a (lift -> genBinder) (lift -> genAnn)
    = runReaderT expression' Set.empty
    where
      expression'
        = Gen.recursive
          Gen.choice
          [ integerExpression genAnn
          , constructorExpression genAnn
          , primitiveExpression genAnn
          , variableExpression genAnn
          ]
          [ applicationExpression genAnn expression'
          , letExpression _a genBinder genAnn expression'
          , matchExpression _a genBinder genAnn expression'
          ]
      {-# INLINABLE expression' #-}
  {-# INLINE expression #-}

integerExpression :: (MonadGen m) => m (Annotation t) -> m (Expression t l a)
integerExpression genAnn = EInteger <$> genAnn <*> Gen.integer
{-# INLINABLE integerExpression #-}

constructorExpression :: (MonadGen m) => m (Annotation t) -> m (Expression t l a)
constructorExpression genAnn = EConstructor <$> genAnn <*> Gen.integer <*> Gen.integer
{-# INLINABLE constructorExpression #-}

primitiveExpression :: (MonadGen m) => m (Annotation t) -> m (Expression t l a)
primitiveExpression genAnn = EPrimitive <$> genAnn <*> Gen.primitive

variableExpression :: (MonadGen m, MonadReader env m, env ~ Set.Set Identifier) => m (Annotation t) -> m (Expression t l a)
variableExpression genAnn = do
  idents <- ask
  EVariable <$> genAnn <*> Gen.element (Set.toList idents)
{-# INLINABLE variableExpression #-}

applicationExpression :: (MonadGen m, MonadReader env m, env ~ Set.Set Identifier, Ord a) => m (Annotation t) -> m (Expression t l a) -> m (Expression t l a)
applicationExpression genAnn genExpr
  = EApplication <$> genAnn <*> genExpr <*> genExpr
{-# INLINABLE applicationExpression #-}

letExpression :: (MonadGen m, MonadReader env m, env ~ Set.Set Identifier, Ord a) => ALens' a Identifier -> m a -> m (Annotation t) -> m (Expression t l a) -> m (Expression t l a)
letExpression _a genBinder genAnn genExpr = do
  ann <- genAnn
  flag <- Gen.element [Recursive, NonRecursive]
  binderSet <- Gen.set (Range.linear 1 100) genBinder
  orderedBinders <- Gen.shuffle (Set.toList binderSet)
  let
    bodyExtraEnv = Set.fromList $ orderedBinders ^.. each . cloneLens _a
    lDefsExtraEnv
      | isRecursive flag = bodyExtraEnv
      | otherwise = Set.empty
  lDefs <- local (lDefsExtraEnv <>) $ traverse (letDefinition genExpr) orderedBinders
  body <- local (bodyExtraEnv <>) genExpr
  pure $ ELet ann flag lDefs body
{-# INLINABLE letExpression #-}

letDefinition :: (MonadGen m, MonadReader env m, env ~ Set.Set Identifier, Ord a) => m (Expression t l a) -> a -> m (LetDefinition t l a)
letDefinition genExpr binder
  = LetDefinition . (,) binder <$> genExpr
{-# INLINABLE letDefinition #-}

matchExpression :: (MonadGen m, MonadReader env m, env ~ Set.Set Identifier, Ord a) => ALens' a Identifier -> m a -> m (Annotation t) -> m (Expression t l a) -> m (Expression t l a)
matchExpression _a genBinder genAnn genExpr = do
  ann <- genAnn
  scrutinee <- genExpr
  tagRange <- Gen.integral (Range.linear 0 100)
  tags <- Gen.subsequence [0..tagRange]
  orderedTags <- Gen.shuffle tags
  matchCases <- traverse (matchCase _a genBinder genExpr) orderedTags
  pure $ EMatch ann scrutinee matchCases
{-# INLINABLE matchExpression #-}

matchCase :: (MonadGen m, MonadReader env m, env ~ Set.Set Identifier, Ord a) => ALens' a Identifier -> m a -> m (Expression t l a) -> Integer -> m (MatchCase t l a)
matchCase _a genBinder genExpr tag = do
  binders <- Set.toList <$> Gen.set (Range.linear 1 100) genBinder
  orderedBinders <- Gen.shuffle binders
  let
    bodyExtraEnv = Set.fromList $ orderedBinders ^.. each . cloneLens _a
  body <- local (bodyExtraEnv <>) genExpr
  pure $ MatchCase (tag, orderedBinders, body)
{-# INLINABLE matchCase #-}

lambdaExpression :: (MonadGen m, MonadReader env m, env ~ Set.Set Identifier, Ord a) => ALens' a Identifier -> m a -> m (Annotation t) -> m (Expression t 'MC a) -> m (Expression t 'MC a)
lambdaExpression _a genBinder genAnn genExpr = do
  ann <- genAnn
  args <- Set.toList <$> Gen.set (Range.linear 1 100) genBinder
  orderedArgs <- Gen.shuffle args
  let
    bodyExtraEnv = Set.fromList $ orderedArgs ^.. each . cloneLens _a
  body <- local (bodyExtraEnv <>) genExpr
  pure $ ELambda ann orderedArgs body
