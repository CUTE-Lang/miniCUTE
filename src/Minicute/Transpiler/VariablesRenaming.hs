{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Minicute.Transpiler.VariablesRenaming
  ( renameVariablesMainL
  ) where

import Control.Lens
import Control.Monad.State
import Control.Monad.Reader
import Minicute.Data.Fix
import Minicute.Types.Minicute.Program

import qualified Data.Map as Map

renameVariablesMainL :: MainProgramL -> MainProgramL
renameVariablesMainL = renameVariablesL id

renameVariablesL :: Lens' a Identifier -> ProgramL a -> ProgramL a
renameVariablesL _a = renameVariables# _a (renameVariablesEL _a)

renameVariables# :: Lens' a Identifier -> Renamer expr -> Program# a expr -> Program# a expr
renameVariables# _a rExpr
  = flip evalState initialIdGeneratorState
    . flip runReaderT initialRenamedRecord
    . traverseOf _supercombinators renameVariablesScs#
  where
    renameVariablesScs# scs = do
      scBinders' <- traverse renameScBinder scBinders
      let
        scBinderRecord = renamedRecordFromIdList (zip scBinders scBinders')
        scs' = zipWith (set _supercombinatorBinder) scBinders' scs
      addLocalRenamedRecord scBinderRecord
        . traverse renameScArgumentsAndBody
        $ scs'
      where
        renameScArgumentsAndBody sc = do
          scArguments' <- traverse (renameA _a) scArguments
          let
            scArgumentRecord = renamedRecordFromAList _a (zip scArguments scArguments')
            sc' = set _supercombinatorArguments scArguments' sc
          addLocalRenamedRecord scArgumentRecord
            . traverseOf _supercombinatorBody rExpr
            $ sc'
          where
            scArguments = view _supercombinatorArguments sc
        scBinders = view _supercombinatorBinder <$> scs

    renameScBinder binder
      | binder == "main" = return binder
      | otherwise = renameIdentifier binder

renameVariablesEL :: Lens' a Identifier -> Renamer (ExpressionL a)
renameVariablesEL _a expr = Fix2 <$> renameVariablesEL# _a (renameVariablesEL _a) (unFix2 expr)

renameVariablesEL# :: Lens' a Identifier -> Renamer (expr_ a) -> Renamer (ExpressionL# expr_ a)
renameVariablesEL# _a rExpr (ELExpression# expr#)
  = ELExpression# <$> renameVariablesE# _a rExpr expr#
renameVariablesEL# _a rExpr (ELLambda# args expr) = do
  args' <- traverse (renameA _a) args
  let
    argEnv = renamedRecordFromAList _a (zip args args')
  expr' <- addLocalRenamedRecord argEnv (rExpr expr)
  return (ELLambda# args' expr')

renameVariablesE# :: Lens' a Identifier -> Renamer (expr_ a) -> Renamer (Expression# expr_ a)
renameVariablesE# _ _ e@(EInteger# _) = return e
renameVariablesE# _ _ e@(EConstructor# _ _) = return e
renameVariablesE# _ _ e@(EVariable# v) = do
  record <- ask
  case Map.lookup v record of
    Just v' -> return (EVariable# v')
    Nothing -> return e
renameVariablesE# _a rExpr (EApplication# e1 e2)
  = EApplication# <$> rExpr e1 <*> rExpr e2
renameVariablesE# _a rExpr (ELet# flag lDefs expr) = do
  record <- ask
  lDefBinders' <- traverse (renameA _a) lDefBinders
  let
    lDefBinderRecord = renamedRecordFromAList _a (zip lDefBinders lDefBinders')
    lDefs' = zipWith (set _letDefinitionBinder) lDefBinders' lDefs
    exprRecord = lDefBinderRecord <> record
    lDefRecord
      | isRecursive flag = exprRecord
      | otherwise = record
  lDefs'' <- setLocalRenamedRecord lDefRecord (traverse renameLDefBodies lDefs')
  expr' <- setLocalRenamedRecord exprRecord (rExpr expr)
  return (ELet# flag lDefs'' expr')
  where
    renameLDefBodies
      = traverseOf _letDefinitionBody rExpr

    lDefBinders = view _letDefinitionBinder <$> lDefs
renameVariablesE# _a rExpr (EMatch# expr mCases) = do
  expr' <- rExpr expr
  mCases' <- traverse renameMCase mCases
  return (EMatch# expr' mCases')
  where
    renameMCase mCase = do
      mCaseArgs' <- traverse (renameA _a) mCaseArgs
      let
        mCaseArgRecord = renamedRecordFromAList _a (zip mCaseArgs mCaseArgs')
        mCase' = set _matchCaseArguments mCaseArgs' mCase
      addLocalRenamedRecord mCaseArgRecord (renameMCaseBody mCase')
      where
        renameMCaseBody
          = traverseOf _matchCaseBody rExpr

        mCaseArgs = view _matchCaseArguments mCase

type Renamer a = a -> ReaderT RenamedRecord (State IdGeneratorState) a

renameA :: Lens' a Identifier -> Renamer a
renameA _a = traverseOf _a renameIdentifier

renameIdentifier :: Renamer Identifier
renameIdentifier = lift . generateId

setLocalRenamedRecord :: RenamedRecord -> ReaderT RenamedRecord (State IdGeneratorState) a -> ReaderT RenamedRecord (State IdGeneratorState) a
setLocalRenamedRecord record = local (const record)

addLocalRenamedRecord :: RenamedRecord -> ReaderT RenamedRecord (State IdGeneratorState) a -> ReaderT RenamedRecord (State IdGeneratorState) a
addLocalRenamedRecord record = local (record <>)

type RenamedRecord = Map.Map Identifier Identifier

initialRenamedRecord :: RenamedRecord
initialRenamedRecord = Map.empty

renamedRecordFromAList :: Lens' a Identifier -> [(a, a)] -> RenamedRecord
renamedRecordFromAList _a = renamedRecordFromIdList . (view (alongside _a _a) <$>)

renamedRecordFromIdList :: [(Identifier, Identifier)] -> RenamedRecord
renamedRecordFromIdList = Map.fromList

type IdGeneratorState = Int

initialIdGeneratorState :: IdGeneratorState
initialIdGeneratorState = 0

nextIdGeneratorState :: IdGeneratorState -> IdGeneratorState
nextIdGeneratorState = (+ 1)

generateId :: Identifier -> State IdGeneratorState Identifier
generateId identifier = do
  st <- get
  put (nextIdGeneratorState st)
  return (identifier <> show st)
