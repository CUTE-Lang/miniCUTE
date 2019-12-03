{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Copyright: (c) 2018-present Junyoung Clare Jang
-- License: BSD 3-Clause
--
-- Transpilers to rename variables to avoid name collision.
module Minicute.Transpilers.VariablesRenaming
  ( renameVariablesMain
  ) where

import Control.Lens.Each
import Control.Lens.Lens ( ALens', cloneLens )
import Control.Lens.Operators
import Control.Lens.Wrapped ( _Wrapped )
import Control.Monad.Reader
import Control.Monad.State
import Data.Function
import Minicute.Data.Minicute.Program

import qualified Data.Map as Map

-- |
-- A transpiler to rename variables in 'MainProgram'
renameVariablesMain :: MainProgram t l -> MainProgram t l
renameVariablesMain = renameVariables id
{-# INLINABLE renameVariablesMain #-}

renameVariables :: ALens' a Identifier -> Program t l a -> Program t l a
renameVariables _a
  = flip evalState initialIdGeneratorState
    . flip runReaderT initialRenamedRecord
    . renameProgram
  where
    renameProgram = _Wrapped %%~ renameScs

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

            renameScBody = _supercombinatorBody %%~ renameVariablesE _a

            {-# INLINABLE renameScBody #-}

        {-# INLINABLE renameScsArgsAndBody #-}

    renameScBinder binder
      | binder == "main" = pure binder
      | otherwise = renameIdentifier binder

    {-# INLINABLE renameScBinder #-}
{-# INLINABLE renameVariables #-}

renameVariablesE :: ALens' a Identifier -> Renamer (Expression t l a)
renameVariablesE _ e@(EInteger _ _) = pure e
renameVariablesE _ e@(EConstructor _ _ _) = pure e
renameVariablesE _ (EVariable ann v)
  = asks (EVariable ann . Map.findWithDefault v v)
renameVariablesE _ e@(EPrimitive _ _) = pure e
renameVariablesE _a (EApplication ann e1 e2)
  = EApplication ann <$> renameVariablesE _a e1 <*> renameVariablesE _a e2
renameVariablesE _a (ELet ann flag lDefs expr) = do
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
    renameExpr = local (const exprRecord) . renameVariablesE _a

    {-# INLINABLE lDefsUpdateBinders #-}
    {-# INLINABLE lDefsBinderRecord #-}
    {-# INLINABLE lDefsRecord #-}
    {-# INLINABLE renameLDefs #-}
    {-# INLINABLE renameExpr #-}
  ELet ann flag <$> renameLDefs lDefs <*> renameExpr expr
  where
    lDefsBinders = lDefs ^.. each . _letDefinitionBinder

    renameLDefsBodies = each . _letDefinitionBody %%~ renameVariablesE _a

    {-# INLINABLE renameLDefsBodies #-}
renameVariablesE _a (EMatch ann expr mCases)
  = EMatch ann <$> renameVariablesE _a expr <*> renameMCases mCases
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

        renameMCaseBody = _matchCaseBody %%~ renameVariablesE _a

        {-# INLINABLE renameMCaseBody #-}

    {-# INLINABLE renameMCases #-}
renameVariablesE _a (ELambda ann args expr) = do
  args' <- renameAs _a args
  let
    argRecord = renamedRecordFromALists _a args args'
    renameExpr = local (argRecord <>) . renameVariablesE _a

    {-# INLINABLE argRecord #-}
    {-# INLINABLE renameExpr #-}
  ELambda ann args' <$> renameExpr expr

type Renamer a = a -> ReaderT RenamedRecord (State IdGeneratorState) a

renameAs :: ALens' a Identifier -> Renamer [a]
renameAs _a = each . cloneLens _a %%~ renameIdentifier
{-# INLINABLE renameAs #-}

renameIdentifier :: Renamer Identifier
renameIdentifier = lift . generateId
{-# INLINABLE renameIdentifier #-}

type RenamedRecord = Map.Map Identifier Identifier

initialRenamedRecord :: RenamedRecord
initialRenamedRecord = Map.empty
{-# INLINABLE initialRenamedRecord #-}

renamedRecordFromALists :: ALens' a Identifier -> [a] -> [a] -> RenamedRecord
renamedRecordFromALists _a = renamedRecordFromIdLists `on` (^.. each . cloneLens _a)
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
  pure (Identifier (n <> show st))
{-# INLINABLE generateId #-}
