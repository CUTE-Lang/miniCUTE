{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Minicute.Transpiler.VariablesRenaming
  ( renameVariablesMainL
  ) where

import Control.Lens.Each
import Control.Lens.Iso ( coerced )
import Control.Lens.Lens ( alongside )
import Control.Lens.Operators
import Control.Lens.Type
import Control.Monad.State
import Control.Monad.Reader
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
    . renameVariables# _a (renameVariablesEL _a)
{-# INLINABLE renameVariablesL #-}

renameVariables# :: Lens' a Identifier -> Renamer (expr a) -> Renamer (Program# a expr)
renameVariables# _a rExpr
  = _supercombinators %%~ renameScs
  where
    renameScs scs = do
      scsBinders' <- traverse renameScBinder scsBinders
      let
        scsUpdateBinders = zipWith (_supercombinatorBinder .~) scsBinders'
        scsBinderRecord = renamedRecordFromIdList (zip scsBinders scsBinders')

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
            scArgRecord = renamedRecordFromAList _a (zip scArgs scArgs')

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
renameVariablesEL _a = coerced %~ renameVariablesEL# _a (renameVariablesEL _a)
{-# INLINABLE renameVariablesEL #-}

renameVariablesEL# :: Lens' a Identifier -> Renamer (expr_ a) -> Renamer (ExpressionL# expr_ a)
renameVariablesEL# _a rExpr (ELExpression# expr#)
  = ELExpression# <$> renameVariablesE# _a rExpr expr#
renameVariablesEL# _a rExpr (ELLambda# args expr) = do
  args' <- renameAs _a args
  let
    argRecord = renamedRecordFromAList _a (zip args args')
    renameExpr = local (argRecord <>) . rExpr

    {-# INLINABLE argRecord #-}
    {-# INLINABLE renameExpr #-}
  ELLambda# args' <$> renameExpr expr

renameVariablesE# :: Lens' a Identifier -> Renamer (expr_ a) -> Renamer (Expression# expr_ a)
renameVariablesE# _ _ e@(EInteger# _) = return e
renameVariablesE# _ _ e@(EConstructor# _ _) = return e
renameVariablesE# _ _ (EVariable# v)
  = asks (EVariable# . Map.findWithDefault v v)
renameVariablesE# _ rExpr (EApplication# e1 e2)
  = EApplication# <$> rExpr e1 <*> rExpr e2
renameVariablesE# _a rExpr (ELet# flag lDefs expr) = do
  record <- ask
  lDefsBinders' <- renameAs _a lDefsBinders
  let
    lDefsUpdateBinders = zipWith (_letDefinitionBinder .~) lDefsBinders'
    lDefsBinderRecord = renamedRecordFromAList _a (zip lDefsBinders lDefsBinders')
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
  ELet# flag <$> renameLDefs lDefs <*> renameExpr expr
  where
    lDefsBinders = lDefs ^.. each . _letDefinitionBinder

    renameLDefsBodies = traverse (_letDefinitionBody %%~ rExpr)

    {-# INLINABLE renameLDefsBodies #-}
renameVariablesE# _a rExpr (EMatch# expr mCases)
  = EMatch# <$> rExpr expr <*> renameMCases mCases
  where
    renameMCases = traverse renameMCase
    renameMCase mCase = do
      mCaseArgs' <- renameAs _a mCaseArgs
      let
        mCaseUpdateArgs = _matchCaseArguments .~ mCaseArgs'
        mCaseArgRecord = renamedRecordFromAList _a (zip mCaseArgs mCaseArgs')

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

renamedRecordFromAList :: Lens' a Identifier -> [(a, a)] -> RenamedRecord
renamedRecordFromAList _a = renamedRecordFromIdList . (^.. each . alongside _a _a)
{-# INLINABLE renamedRecordFromAList #-}

renamedRecordFromIdList :: [(Identifier, Identifier)] -> RenamedRecord
renamedRecordFromIdList = Map.fromList
{-# INLINABLE renamedRecordFromIdList #-}

type IdGeneratorState = Int

initialIdGeneratorState :: IdGeneratorState
initialIdGeneratorState = 0
{-# INLINABLE initialIdGeneratorState #-}

nextIdGeneratorState :: IdGeneratorState -> IdGeneratorState
nextIdGeneratorState = (+ 1)
{-# INLINABLE nextIdGeneratorState #-}

generateId :: Identifier -> State IdGeneratorState Identifier
generateId identifier = do
  st <- get
  put (nextIdGeneratorState st)
  return (identifier <> show st)
{-# INLINABLE generateId #-}
