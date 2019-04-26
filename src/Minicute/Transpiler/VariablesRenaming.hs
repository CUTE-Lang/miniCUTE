{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Minicute.Transpiler.VariablesRenaming
  ( renameVariablesMainL
  ) where

import Control.Lens
import Control.Monad.State
import Control.Monad.Reader
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
renameVariablesEL _ e@(ELInteger _) = return e
renameVariablesEL _ e@(ELConstructor _ _) = return e
renameVariablesEL _ e@(ELVariable v) = do
  record <- ask
  case Map.lookup v record of
    Just v' -> return (ELVariable v')
    Nothing -> return e
renameVariablesEL _a (ELApplication e1 e2)
  = ELApplication <$> renameVariablesEL _a e1 <*> renameVariablesEL _a e2
renameVariablesEL _a (ELLet flag lDefs expr) = do
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
  expr' <- setLocalRenamedRecord exprRecord (renameVariablesEL _a expr)
  return (ELLet flag lDefs'' expr')
  where
    renameLDefBodies
      = traverseOf _letDefinitionBody (renameVariablesEL _a)


    lDefBinders = view _letDefinitionBinder <$> lDefs
renameVariablesEL _a (ELMatch expr mCases) = do
  expr' <- renameVariablesEL _a expr
  mCases' <- traverse renameMCase mCases
  return (ELMatch expr' mCases')
  where
    renameMCase mCase = do
      mCaseArgs' <- traverse (renameA _a) mCaseArgs
      let
        mCaseArgRecord = renamedRecordFromAList _a (zip mCaseArgs mCaseArgs')
        mCase' = set _matchCaseArguments mCaseArgs' mCase
      addLocalRenamedRecord mCaseArgRecord (renameMCaseBody mCase')
      where
        renameMCaseBody
          = traverseOf _matchCaseBody (renameVariablesEL _a)

        mCaseArgs = view _matchCaseArguments mCase
renameVariablesEL _a (ELLambda args expr) = do
  args' <- traverse (renameA _a) args
  let
    argEnv = renamedRecordFromAList _a (zip args args')
  expr' <- addLocalRenamedRecord argEnv (renameVariablesEL _a expr)
  return (ELLambda args' expr')

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
