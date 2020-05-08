{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
-- |
-- Copyright: (c) 2018-present Junyoung Clare Jang
-- License: BSD 3-Clause
module Minicute.Test.Minicute.Gen
  ( GenExpressionLevel

  , mainProgram
  , program

  , mainSupercombinator
  , supercombinator

  , mainExpression
  , expression
  ) where

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

type GenExpressionLevel l = GenExpressionLevel' l


mainProgram :: (MonadGen m, GenExpressionLevel l) => m (Annotation t) -> m (MainProgram t l)
mainProgram = program id Gen.identifier
{-# INLINE mainProgram #-}

program :: (MonadGen m, GenExpressionLevel l, Ord a) => ALens' a Identifier -> m a -> m (Annotation t) -> m (Program t l a)
program _a (lift -> getBinder) (lift -> getAnn)
  = runReaderT (programHelper _a getBinder getAnn) Set.empty
{-# INLINE program #-}

programHelper :: (MonadGen m, MonadReader env m, env ~ Set.Set Identifier, GenExpressionLevel l, Ord a) => ALens' a Identifier -> m a -> m (Annotation t) -> m (Program t l a)
programHelper _a genBinder genAnn = do
  identifiers <- uniqueShuffledListOf (Range.linear 1 10) Gen.identifier
  let
    scsExtraEnv = Set.fromList identifiers
  scs <-
    local (scsExtraEnv <>)
    $ traverse (supercombinatorFromIdentifierHelper _a genBinder genAnn) identifiers
  pure $ Program scs
{-# INLINE programHelper #-}

mainSupercombinator :: (MonadGen m, GenExpressionLevel l) => m (Annotation t) -> m (MainSupercombinator t l)
mainSupercombinator = supercombinator id Gen.identifier
{-# INLINE mainSupercombinator #-}

supercombinator :: (MonadGen m, GenExpressionLevel l, Ord a) => ALens' a Identifier -> m a -> m (Annotation t) -> m (Supercombinator t l a)
supercombinator _a (lift -> genBinder) (lift -> genAnn) = do
  ident <- Gen.identifier
  runReaderT (supercombinatorFromIdentifierHelper _a genBinder genAnn ident) (Set.singleton ident)
{-# INLINE supercombinator #-}

supercombinatorFromIdentifierHelper :: (MonadGen m, MonadReader env m, env ~ Set.Set Identifier, GenExpressionLevel l, Ord a) => ALens' a Identifier -> m a -> m (Annotation t) -> Identifier -> m (Supercombinator t l a)
supercombinatorFromIdentifierHelper _a genBinder genAnn ident = do
  args <- uniqueShuffledListOf (Range.linear 1 100) genBinder
  let
    bodyExtraEnv = Set.fromList $ args ^.. each . cloneLens _a
  body <- local (bodyExtraEnv <>) $ expressionHelper _a genBinder genAnn
  pure $ Supercombinator (ident, args, body)
{-# INLINE supercombinatorFromIdentifierHelper #-}

mainExpression :: (MonadGen m, GenExpressionLevel l) => m (Annotation t) -> m (MainExpression t l)
mainExpression = expression id Gen.identifier
{-# INLINE mainExpression #-}

expression :: (MonadGen m, GenExpressionLevel l, Ord a) => ALens' a Identifier -> m a -> m (Annotation t) -> m (Expression t l a)
expression _a (lift -> genBinder) (lift -> genAnn)
  = runReaderT (expressionHelper _a genBinder genAnn) Set.empty
{-# INLINE expression #-}


class GenExpressionLevel' (l :: ExpressionLevel) where
  expressionHelper :: (MonadGen m, MonadReader env m, env ~ Set.Set Identifier, Ord a) => ALens' a Identifier -> m a -> m (Annotation t) -> m (Expression t l a)

instance GenExpressionLevel' 'MC where
  expressionHelper _a genBinder genAnn
    = Gen.recursive
      Gen.choice
      [ integerExpression genAnn
      , constructorExpression genAnn
      , primitiveExpression genAnn
      , variableExpression genAnn
      ]
      [ applicationExpression genAnn (expressionHelper _a genBinder genAnn)
      , letExpression _a genBinder genAnn (expressionHelper _a genBinder genAnn)
      , matchExpression _a genBinder genAnn (expressionHelper _a genBinder genAnn)
      , lambdaExpression _a genBinder genAnn (expressionHelper _a genBinder genAnn)
      ]
  {-# INLINE expressionHelper #-}

instance GenExpressionLevel' 'LLMC where
  expressionHelper _a genBinder genAnn
    = Gen.recursive
      Gen.choice
      [ integerExpression genAnn
      , constructorExpression genAnn
      , primitiveExpression genAnn
      , variableExpression genAnn
      ]
      [ applicationExpression genAnn (expressionHelper _a genBinder genAnn)
      , letExpression _a genBinder genAnn (expressionHelper _a genBinder genAnn)
      , matchExpression _a genBinder genAnn (expressionHelper _a genBinder genAnn)
      ]
  {-# INLINE expressionHelper #-}


integerExpression :: (MonadGen m) => m (Annotation t) -> m (Expression t l a)
integerExpression genAnn = EInteger <$> genAnn <*> Gen.integer
{-# INLINE integerExpression #-}

constructorExpression :: (MonadGen m) => m (Annotation t) -> m (Expression t l a)
constructorExpression genAnn = EConstructor <$> genAnn <*> Gen.integer <*> Gen.integer
{-# INLINE constructorExpression #-}

primitiveExpression :: (MonadGen m) => m (Annotation t) -> m (Expression t l a)
primitiveExpression genAnn = EPrimitive <$> genAnn <*> Gen.primitive
{-# INLINE primitiveExpression #-}

variableExpression :: (MonadGen m, MonadReader env m, env ~ Set.Set Identifier) => m (Annotation t) -> m (Expression t l a)
variableExpression genAnn = do
  idents <- ask
  EVariable <$> genAnn <*> Gen.element (Set.toList idents)
{-# INLINE variableExpression #-}

applicationExpression :: (MonadGen m, MonadReader env m, env ~ Set.Set Identifier, Ord a) => m (Annotation t) -> m (Expression t l a) -> m (Expression t l a)
applicationExpression genAnn genExpr
  = EApplication <$> genAnn <*> genExpr <*> genExpr
{-# INLINABLE applicationExpression #-}

letExpression :: (MonadGen m, MonadReader env m, env ~ Set.Set Identifier, Ord a) => ALens' a Identifier -> m a -> m (Annotation t) -> m (Expression t l a) -> m (Expression t l a)
letExpression _a genBinder genAnn genExpr = do
  ann <- genAnn
  flag <- Gen.element [Recursive, NonRecursive]
  binders <- uniqueShuffledListOf (Range.linear 1 100) genBinder
  let
    bodyExtraEnv = Set.fromList $ binders ^.. each . cloneLens _a
    lDefsExtraEnv
      | isRecursive flag = bodyExtraEnv
      | otherwise = Set.empty
  lDefs <- local (lDefsExtraEnv <>) $ traverse (letDefinitionFromBinder genExpr) binders
  body <- local (bodyExtraEnv <>) genExpr
  pure $ ELet ann flag lDefs body
{-# INLINABLE letExpression #-}

letDefinitionFromBinder :: (MonadGen m, MonadReader env m, env ~ Set.Set Identifier, Ord a) => m (Expression t l a) -> a -> m (LetDefinition t l a)
letDefinitionFromBinder genExpr binder
  = LetDefinition . (,) binder <$> genExpr
{-# INLINABLE letDefinitionFromBinder #-}

matchExpression :: (MonadGen m, MonadReader env m, env ~ Set.Set Identifier, Ord a) => ALens' a Identifier -> m a -> m (Annotation t) -> m (Expression t l a) -> m (Expression t l a)
matchExpression _a genBinder genAnn genExpr = do
  ann <- genAnn
  scrutinee <- genExpr
  tagRange <- Gen.integral (Range.linear 0 100)
  tags <- Gen.subsequence [0..tagRange]
  orderedTags <- Gen.shuffle tags
  mCases <- traverse (matchCaseOfTag _a genBinder genExpr) orderedTags
  pure $ EMatch ann scrutinee mCases
{-# INLINABLE matchExpression #-}

matchCaseOfTag :: (MonadGen m, MonadReader env m, env ~ Set.Set Identifier, Ord a) => ALens' a Identifier -> m a -> m (Expression t l a) -> Integer -> m (MatchCase t l a)
matchCaseOfTag _a genBinder genExpr tag = do
  binders <- uniqueShuffledListOf (Range.linear 1 100) genBinder
  let
    bodyExtraEnv = Set.fromList $ binders ^.. each . cloneLens _a
  body <- local (bodyExtraEnv <>) genExpr
  pure $ MatchCase (tag, binders, body)
{-# INLINABLE matchCaseOfTag #-}

lambdaExpression :: (MonadGen m, MonadReader env m, env ~ Set.Set Identifier, Ord a) => ALens' a Identifier -> m a -> m (Annotation t) -> m (Expression t 'MC a) -> m (Expression t 'MC a)
lambdaExpression _a genBinder genAnn genExpr = do
  ann <- genAnn
  args <- uniqueShuffledListOf (Range.linear 1 100) genBinder
  let
    bodyExtraEnv = Set.fromList $ args ^.. each . cloneLens _a
  body <- local (bodyExtraEnv <>) genExpr
  pure $ ELambda ann args body
{-# INLINABLE lambdaExpression #-}


uniqueShuffledListOf :: (MonadGen m, Ord a) => Range.Range Int -> m a -> m [a]
uniqueShuffledListOf range genItem = do
  items <- Set.toList <$> Gen.set range genItem
  Gen.shuffle items
{-# INLINE uniqueShuffledListOf #-}
