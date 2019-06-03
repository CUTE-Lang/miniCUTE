{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Minicute.Transpilers.VariablesRenaming
  ( renameVariablesMainL
  ) where

import Control.Lens.Each
import Control.Lens.Iso ( coerced )
import Control.Lens.Operators
import Control.Lens.Type
import Control.Lens.Wrapped ( _Wrapped )
import Control.Monad.State
import Control.Monad.Reader
import Data.Function
import Minicute.Data.Fix
import Minicute.Types.Minicute.Program

import qualified Data.Map as Map

renameVariablesMainL :: MainProgramL -> MainProgramL
renameVariablesMainL = renameVariablesL id
{-# INLINABLE renameVariablesMainL #-}

renameVariablesL :: Lens' a Identifier -> ProgramL a -> ProgramL a
renameVariablesL _a
  = flip evalState initialIdGeneratorState
    . flip runReaderT initialRenamedRecord
    . renameVariables_ _a (renameVariablesEL _a)
{-# INLINABLE renameVariablesL #-}

renameVariables_ :: Lens' a Identifier -> Renamer (expr a) -> Renamer (Program_ expr a)
renameVariables_ _a rExpr
  = _Wrapped %%~ renameScs
  where
    renameScs scs = do
      scsBinders' <- traverse renameScBinder scsBinders
      let
        scsUpdateBinders = zipWith (_supercombinatorBinder .~) scsBinders'
        scsBinderRecord = renamedRecordFromIdLists scsBinders scsBinders'

        {-# INLINABLE scsUpdateBinders #-}
        {-# INLINABLE scsBinderRecord #-}
      local (scsBinderRecord <>) . renameScsArgsAndBody . scsUpdateBinders $ scs
      where
        scsBinders = scs ^.. each . _supercombinatorBinder

        renameScsArgsAndBody = traverse renameScArgsAndBody
        renameScArgsAndBody sc = do
          scArgs' <- renameAs _a scArgs
          let
            scUpdateArgs = _supercombinatorArguments .~ scArgs'
            scArgRecord = renamedRecordFromALists _a scArgs scArgs'

            {-# INLINABLE scUpdateArgs #-}
            {-# INLINABLE scArgRecord #-}
          local (scArgRecord <>) . renameScBody . scUpdateArgs $ sc
          where
            scArgs = sc ^. _supercombinatorArguments

            renameScBody = _supercombinatorBody %%~ rExpr

            {-# INLINABLE renameScBody #-}

        {-# INLINABLE renameScsArgsAndBody #-}

    renameScBinder binder
      | binder == "main" = return binder
      | otherwise = renameIdentifier binder

    {-# INLINABLE renameScBinder #-}

renameVariablesEL :: Lens' a Identifier -> Renamer (ExpressionL a)
renameVariablesEL _a = coerced %~ renameVariablesEL_ _a (renameVariablesEL _a)
{-# INLINABLE renameVariablesEL #-}

renameVariablesEL_ :: Lens' a Identifier -> Renamer (expr_ a) -> Renamer (ExpressionL_ expr_ a)
renameVariablesEL_ _a rExpr (ELExpression_ expr_)
  = ELExpression_ <$> renameVariablesE_ _a rExpr expr_
renameVariablesEL_ _a rExpr (ELLambda_ args expr) = do
  args' <- renameAs _a args
  let
    argRecord = renamedRecordFromALists _a args args'
    renameExpr = local (argRecord <>) . rExpr

    {-# INLINABLE argRecord #-}
    {-# INLINABLE renameExpr #-}
  ELLambda_ args' <$> renameExpr expr

renameVariablesE_ :: Lens' a Identifier -> Renamer (expr_ a) -> Renamer (Expression_ expr_ a)
renameVariablesE_ _ _ e@(EInteger_ _) = return e
renameVariablesE_ _ _ e@(EConstructor_ _ _) = return e
renameVariablesE_ _ _ (EVariable_ v)
  = asks (EVariable_ . Map.findWithDefault v v)
renameVariablesE_ _ rExpr (EApplication_ e1 e2)
  = EApplication_ <$> rExpr e1 <*> rExpr e2
renameVariablesE_ _a rExpr (ELet_ flag lDefs expr) = do
  record <- ask
  lDefsBinders' <- renameAs _a lDefsBinders
  let
    lDefsUpdateBinders = zipWith (_letDefinitionBinder .~) lDefsBinders'
    lDefsBinderRecord = renamedRecordFromALists _a lDefsBinders lDefsBinders'
    -- |
    -- Execute '<>' once.
    -- '<>' on 'Map.Map' takes O(m*log(n/m + 1)), i.e., not that cheap.
    exprRecord = lDefsBinderRecord <> record
    lDefsRecord
      | isRecursive flag = exprRecord
      | otherwise = record
    renameLDefs = local (const lDefsRecord) . renameLDefsBodies . lDefsUpdateBinders
    renameExpr = local (const exprRecord) . rExpr

    {-# INLINABLE lDefsUpdateBinders #-}
    {-# INLINABLE lDefsBinderRecord #-}
    {-# INLINABLE lDefsRecord #-}
    {-# INLINABLE renameLDefs #-}
    {-# INLINABLE renameExpr #-}
  ELet_ flag <$> renameLDefs lDefs <*> renameExpr expr
  where
    lDefsBinders = lDefs ^.. each . _letDefinitionBinder

    renameLDefsBodies = each . _letDefinitionBody %%~ rExpr

    {-# INLINABLE renameLDefsBodies #-}
renameVariablesE_ _a rExpr (EMatch_ expr mCases)
  = EMatch_ <$> rExpr expr <*> renameMCases mCases
  where
    renameMCases = traverse renameMCase
    renameMCase mCase = do
      mCaseArgs' <- renameAs _a mCaseArgs
      let
        mCaseUpdateArgs = _matchCaseArguments .~ mCaseArgs'
        mCaseArgRecord = renamedRecordFromALists _a mCaseArgs mCaseArgs'

        {-# INLINABLE mCaseUpdateArgs #-}
        {-# INLINABLE mCaseArgRecord #-}
      local (mCaseArgRecord <>) . renameMCaseBody . mCaseUpdateArgs $ mCase
      where
        mCaseArgs = mCase ^. _matchCaseArguments

        renameMCaseBody = _matchCaseBody %%~ rExpr

        {-# INLINABLE renameMCaseBody #-}

    {-# INLINABLE renameMCases #-}

type Renamer a = a -> ReaderT RenamedRecord (State IdGeneratorState) a

renameAs :: Traversal' a Identifier -> Renamer [a]
renameAs _a = each . _a %%~ renameIdentifier
{-# INLINABLE renameAs #-}

renameIdentifier :: Renamer Identifier
renameIdentifier = lift . generateId
{-# INLINABLE renameIdentifier #-}

type RenamedRecord = Map.Map Identifier Identifier

initialRenamedRecord :: RenamedRecord
initialRenamedRecord = Map.empty
{-# INLINABLE initialRenamedRecord #-}

renamedRecordFromALists :: Lens' a Identifier -> [a] -> [a] -> RenamedRecord
renamedRecordFromALists _a = renamedRecordFromIdLists `on` (^.. each . _a)
{-# INLINABLE renamedRecordFromALists #-}

renamedRecordFromIdLists :: [Identifier] -> [Identifier] -> RenamedRecord
renamedRecordFromIdLists = (Map.fromList .) . zip
{-# INLINABLE renamedRecordFromIdLists #-}

type IdGeneratorState = Int

initialIdGeneratorState :: IdGeneratorState
initialIdGeneratorState = 0
{-# INLINABLE initialIdGeneratorState #-}

nextIdGeneratorState :: IdGeneratorState -> IdGeneratorState
nextIdGeneratorState = (+ 1)
{-# INLINABLE nextIdGeneratorState #-}

generateId :: Identifier -> State IdGeneratorState Identifier
generateId (Identifier n) = do
  st <- get
  put (nextIdGeneratorState st)
  return (Identifier (n <> show st))
{-# INLINABLE generateId #-}
