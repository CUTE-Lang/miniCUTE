{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Copyright: (c) 2018-present Junyoung Clare Jang
-- License: BSD 3-Clause
--
-- Transpilers to rename variables to avoid name collision.
module Minicute.Transpilers.VariablesRenaming
  ( module Minicute.Data.Minicute.Program

  , renameVariablesMain
  ) where

import Control.Arrow ( first )
import Control.Lens.Each
import Control.Lens.Lens ( ALens', cloneLens )
import Control.Lens.Operators
import Control.Lens.Traversal ( partsOf )
import Control.Lens.Wrapped ( _Wrapped )
import Control.Monad.Reader
import Control.Monad.State
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
      (scsBinderRecord, scs') <-
        first renamedRecordFromIdentifierPairList
        . sequence
        <$> traverse renameScBinder scs
      local (scsBinderRecord <>) $ renameScsArgsAndBody scs'
      where
        renameScsArgsAndBody = traverse renameScArgsAndBody
        renameScArgsAndBody sc = do
          (scArgRecord, sc') <-
            first renamedRecordFromIdentifierPairList
            <$> renameAsIn _supercombinatorArguments _a sc
          local (scArgRecord <>) $ renameScBody sc'
          where
            renameScBody = _supercombinatorBody %%~ renameVariablesE _a

            {-# INLINABLE renameScBody #-}
        {-# INLINABLE renameScsArgsAndBody #-}

    renameScBinder sc
      | sc ^. _supercombinatorBinder == "main" = pure ([], sc)
      | otherwise = first pure <$> renameAIn _supercombinatorBinder id sc

    {-# INLINE renameProgram #-}
    {-# INLINABLE renameScBinder #-}
{-# INLINABLE renameVariables #-}

renameVariablesE :: ALens' a Identifier -> Renamer' (Expression t l a)
renameVariablesE _ e@(EInteger _ _) = pure e
renameVariablesE _ e@(EConstructor _ _ _) = pure e
renameVariablesE _ (EVariable ann v)
  = asks (EVariable ann . Map.findWithDefault v v)
renameVariablesE _ e@(EPrimitive _ _) = pure e
renameVariablesE _a (EApplication ann e1 e2)
  = EApplication ann <$> renameVariablesE _a e1 <*> renameVariablesE _a e2
renameVariablesE _a (ELet ann flag lDefs expr) = do
  (lDefsBinderRecord, lDefs') <-
    first renamedRecordFromIdentifierPairList
    . sequence
    <$> traverse (fmap (first pure) . renameAIn _letDefinitionBinder _a) lDefs
  (lDefsRecord, exprRecord) <- asks (getRecords lDefsBinderRecord)
  ELet ann flag
    <$> renameLDefs lDefsRecord lDefs'
    <*> renameExpr exprRecord expr
  where
    getRecords lDefsBinderRecord record = (lDefsRecord, exprRecord)
      where
        lDefsRecord
          | isRecursive flag = exprRecord
          | otherwise = record
        exprRecord = lDefsBinderRecord <> record

        {-# INLINABLE lDefsRecord #-}
        {-# INLINABLE exprRecord #-}

    renameLDefs lDefsRecord
      = local (const lDefsRecord)
        . renameLDefsBodies
    renameLDefsBodies = each . _letDefinitionBody %%~ renameVariablesE _a
    renameExpr exprRecord = local (const exprRecord) . renameVariablesE _a

    {-# INLINABLE getRecords #-}
    {-# INLINABLE renameLDefs #-}
    {-# INLINABLE renameLDefsBodies #-}
    {-# INLINABLE renameExpr #-}
renameVariablesE _a (EMatch ann expr mCases)
  = EMatch ann <$> renameVariablesE _a expr <*> renameMCases mCases
  where
    renameMCases = traverse renameMCase
    renameMCase mCase = do
      (mCaseArgRecord, mCase') <-
        first renamedRecordFromIdentifierPairList
        <$> renameAsIn _matchCaseArguments _a mCase
      local (mCaseArgRecord <>) $ renameMCaseBody mCase'
      where
        renameMCaseBody = _matchCaseBody %%~ renameVariablesE _a

        {-# INLINABLE renameMCaseBody #-}
    {-# INLINABLE renameMCases #-}
renameVariablesE _a (ELambda ann args expr) = do
  (argRecord, args') <-
    first renamedRecordFromIdentifierPairList
    <$> renameAsIn id _a args
  expr' <- local (argRecord <>) $ renameVariablesE _a expr
  pure $ ELambda ann args' expr'

type Renamer a b = a -> ReaderT RenamedRecord (State IdGeneratorState) b

type Renamer' a = Renamer a a

renameIdentifier :: Renamer' Identifier
renameIdentifier = lift . generateId
{-# INLINABLE renameIdentifier #-}

renameIdentifierIn :: ALens' s Identifier -> Renamer s ((Identifier, Identifier), s)
renameIdentifierIn _s s = do
  ident' <- renameIdentifier ident
  pure ((ident, ident'), s & _s #~ ident')
  where
    ident = s ^# _s

    {-# INLINABLE ident #-}
{-# INLINABLE renameIdentifierIn #-}

renameIdentifiers :: Renamer' [Identifier]
renameIdentifiers = traverse renameIdentifier
{-# INLINABLE renameIdentifiers #-}

renameIdentifiersIn :: ALens' s [Identifier] -> Renamer s ([(Identifier, Identifier)], s)
renameIdentifiersIn _s s = do
  idents' <- renameIdentifiers idents
  pure (zip idents idents', s & _s #~ idents')
  where
    idents = s ^# _s

    {-# INLINABLE idents #-}
{-# INLINABLE renameIdentifiersIn #-}

renameAIn :: ALens' s a -> ALens' a Identifier -> Renamer s ((Identifier, Identifier), s)
renameAIn _s _a = renameIdentifierIn (cloneLens _s . cloneLens _a)
{-# INLINABLE renameAIn #-}

renameAsIn :: ALens' s [a] -> ALens' a Identifier -> Renamer s ([(Identifier, Identifier)], s)
renameAsIn _s _a = renameIdentifiersIn (cloneLens _s . partsOf (each . cloneLens _a))
{-# INLINABLE renameAsIn #-}

type RenamedRecord = Map.Map Identifier Identifier

initialRenamedRecord :: RenamedRecord
initialRenamedRecord = Map.empty
{-# INLINABLE initialRenamedRecord #-}

renamedRecordFromIdentifierPairList :: [(Identifier, Identifier)] -> RenamedRecord
renamedRecordFromIdentifierPairList = Map.fromList
{-# INLINABLE renamedRecordFromIdentifierPairList #-}

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
