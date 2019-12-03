{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
-- |
-- Copyright: (c) 2018-present Junyoung Clare Jang
-- License: BSD 3-Clause
--
-- Transpilers to extract free variable information of expressions
module Minicute.Transpilers.FreeVariables
  ( FreeVariables

  , formFreeVariablesMainMC
  , formFreeVariablesMC
  ) where

import Control.Lens.Each
import Control.Lens.Getter ( Getting, to )
import Control.Lens.Operators
import Control.Lens.Type
import Control.Lens.Wrapped ( _Wrapped )
import Control.Monad.Reader
import Minicute.Data.Minicute.Program

import qualified Data.Set as Set
import qualified Data.Set.Lens as Set

-- |
-- A set of identifiers that are free in the annotated expression
type FreeVariables = Set.Set Identifier

-- |
-- A transpiler to create free variable information for 'MainProgramMC'
formFreeVariablesMainMC :: MainProgram 'Simple 'MC -> MainProgram ('AnnotatedWith FreeVariables) 'MC
formFreeVariablesMainMC = formFreeVariablesMC id
{-# INLINEABLE formFreeVariablesMainMC #-}

-- |
-- A transpiler to create free variable information for 'ProgramMC'
formFreeVariablesMC :: Getter a Identifier -> Program 'Simple 'MC a -> Program ('AnnotatedWith FreeVariables) 'MC a
formFreeVariablesMC _a
  = _Wrapped . each %~ formFreeVariablesSc
    where
      formFreeVariablesSc sc
        = sc & _supercombinatorBody %~ flip runReader scArgsSet . formFVsEMC _a
        where
          scArgsSet = sc ^. _supercombinatorArguments . setFrom (each . _a)

      {-# INLINEABLE formFreeVariablesSc #-}
{-# INLINEABLE formFreeVariablesMC #-}

-- |
-- Set of identifiers those are candidates of free variables
type FVELEnvironment = Set.Set Identifier

type FVFormer e e' = e -> Reader FVELEnvironment e'

formFVsEMC :: Getter a Identifier -> FVFormer (Expression 'Simple 'MC a) (Expression ('AnnotatedWith FreeVariables) 'MC a)
formFVsEMC _ (EInteger _ n) = pure (EInteger Set.empty n)
formFVsEMC _ (EConstructor _ tag arity) = pure (EConstructor Set.empty tag arity)
formFVsEMC _ (EVariable _ v) = do
  env <- ask

  let
    fvs
      | Set.member v env = Set.singleton v
      | otherwise = Set.empty

    {-# INLINEABLE fvs #-}
  pure (EVariable fvs v)
formFVsEMC _ (EPrimitive _ prim) = pure (EPrimitive Set.empty prim)
formFVsEMC _a (EApplication _ expr1 expr2) = do
  expr1' <- formFVsEMC _a expr1
  expr2' <- formFVsEMC _a expr2

  let
    fvs = expr1' ^. _annotation <> expr2' ^. _annotation

    {-# INLINEABLE fvs #-}
  pure (EApplication fvs expr1' expr2')
formFVsEMC _a (ELet _ flag lDefs expr) = do
  env <- ask

  let
    exprEnv = lDefsBinderIdSet <> env
    lDefsEnv
      | isRecursive flag = exprEnv
      | otherwise = env

    formLDefsBodies = each . _letDefinitionBody %%~ formFVsEMC _a

    {-# INLINEABLE lDefsEnv #-}
    {-# INLINEABLE formLDefsBodies #-}
  expr' <- local (const exprEnv) . formFVsEMC _a $ expr
  lDefs' <- local (const lDefsEnv) . formLDefsBodies $ lDefs

  let
    fvsLDefsBodies' = lDefs' ^. each . _letDefinitionBody . _annotation
    fvsLDefs'
      | isRecursive flag = fvsLDefsBodies' Set.\\ lDefsBinderIdSet
      | otherwise = fvsLDefsBodies'
    fvsExpr' = (expr' ^. _annotation) Set.\\ lDefsBinderIdSet
    fvs = fvsLDefs' <> fvsExpr'

    {-# INLINEABLE fvsLDefsBodies' #-}
    {-# INLINEABLE fvsLDefs' #-}
    {-# INLINEABLE fvsExpr' #-}
    {-# INLINEABLE fvs #-}
  pure (ELet fvs flag lDefs' expr')
  where
    lDefsBinderIdSet = lDefs ^. setFrom (each . _letDefinitionBinder . _a)
formFVsEMC _a (EMatch _ expr mCases) = do
  let
    formMCaseBodies mCaseArgSet = local (mCaseArgSet <>) . formFVsEMC _a
    formMCase mCaseArgSet = _matchCaseBody %%~ formMCaseBodies mCaseArgSet

    {-# INLINEABLE formMCaseBodies #-}
    {-# INLINEABLE formMCase #-}
  expr' <- formFVsEMC _a expr
  mCases' <- zipWithM formMCase mCasesArgumentSets mCases

  let
    fvssMCasesBodies' = mCases' ^.. each . _matchCaseBody . _annotation
    fvsMCases' = mconcat (zipWith (Set.\\) fvssMCasesBodies' mCasesArgumentSets)
    fvs = fvsMCases' <> expr' ^. _annotation

    {-# INLINEABLE fvssMCasesBodies' #-}
    {-# INLINEABLE fvsMCases' #-}
    {-# INLINEABLE fvs #-}
  pure (EMatch fvs expr' mCases')
  where
    mCasesArgumentSets = mCases ^.. each . _matchCaseArguments . setFrom (each . _a)
formFVsEMC _a (ELambda _ args expr) = do
  expr' <- local (argIdSet <>) . formFVsEMC _a $ expr

  let
    fvsExpr' = expr' ^. _annotation
    fvs = fvsExpr' Set.\\ argIdSet

    {-# INLINEABLE fvsExpr' #-}
    {-# INLINEABLE fvs #-}
  pure (ELambda fvs args expr')
  where
    argIdSet = args ^. setFrom (each . _a)

-- |
-- __TODO: move this definition into a separate utility module.__
setFrom :: Getting (Set.Set a) s a -> Getter s (Set.Set a)
setFrom = to . Set.setOf
{-# INLINABLE setFrom #-}
