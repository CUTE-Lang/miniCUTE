{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
-- |
-- Transpilers to extract free variable information of expressions
module Minicute.Transpilers.FreeVariables
  ( ProgramMCWithFreeVariables
  , MainProgramMCWithFreeVariables
  , ExpressionMCWithFreeVariables
  , MainExpressionMCWithFreeVariables

  , FreeVariables

  , formFreeVariablesMainMC
  , formFreeVariablesMC
  ) where

import Control.Lens.Each
import Control.Lens.Getter ( Getting, to )
import Control.Lens.Operators
import Control.Lens.Type
import Control.Lens.Wrapped ( _Wrapped )
import Control.Monad.Reader
import Minicute.Data.Minicute.Annotated

import qualified Data.Set as Set
import qualified Data.Set.Lens as Set

-- |
-- 'ProgramMC' annotated with 'FreeVariables'
type ProgramMCWithFreeVariables = AnnotatedProgramMC FreeVariables
-- |
-- 'MainProgramMC' annotated with 'FreeVariables'
type MainProgramMCWithFreeVariables = MainAnnotatedProgramMC FreeVariables
-- |
-- 'ExpressionMC' annotated with 'FreeVariables'
type ExpressionMCWithFreeVariables = AnnotatedExpressionMC FreeVariables
-- |
-- 'MainExpressionMC' annotated with 'FreeVariables'
type MainExpressionMCWithFreeVariables = MainAnnotatedExpressionMC FreeVariables

-- |
-- A set of identifiers that are free in the annotated expression
type FreeVariables = Set.Set Identifier

-- |
-- A transpiler to create free variable information for 'MainProgramMC'
formFreeVariablesMainMC :: MainProgramMC -> MainProgramMCWithFreeVariables
formFreeVariablesMainMC = formFreeVariablesMC id
{-# INLINEABLE formFreeVariablesMainMC #-}

-- |
-- A transpiler to create free variable information for 'ProgramMC'
formFreeVariablesMC :: Getter a Identifier -> ProgramMC a -> ProgramMCWithFreeVariables a
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

formFVsEMC :: Getter a Identifier -> FVFormer (ExpressionMC a) (ExpressionMCWithFreeVariables a)
formFVsEMC _ (EInteger n) = return (AEInteger Set.empty n)
formFVsEMC _ (EConstructor tag arity) = return (AEConstructor Set.empty tag arity)
formFVsEMC _ (EVariable v) = do
  env <- ask

  let
    fvs
      | Set.member v env = Set.singleton v
      | otherwise = Set.empty

    {-# INLINEABLE fvs #-}
  return (AEVariable fvs v)
formFVsEMC _a (EApplication expr1 expr2) = do
  expr1' <- formFVsEMC _a expr1
  expr2' <- formFVsEMC _a expr2

  let
    fvs = expr1' ^. _annotation <> expr2' ^. _annotation

    {-# INLINEABLE fvs #-}
  return (AEApplication fvs expr1' expr2')
formFVsEMC _a (ELet flag lDefs expr) = do
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
  return (AELet fvs flag lDefs' expr')
  where
    lDefsBinderIdSet = lDefs ^. setFrom (each . _letDefinitionBinder . _a)
formFVsEMC _a (EMatch expr mCases) = do
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
  return (AEMatch fvs expr' mCases')
  where
    mCasesArgumentSets = mCases ^.. each . _matchCaseArguments . setFrom (each . _a)
formFVsEMC _a (ELambda args expr) = do
  expr' <- local (argIdSet <>) . formFVsEMC _a $ expr

  let
    fvsExpr' = expr' ^. _annotation
    fvs = fvsExpr' Set.\\ argIdSet

    {-# INLINEABLE fvsExpr' #-}
    {-# INLINEABLE fvs #-}
  return (AELambda fvs args expr')
  where
    argIdSet = args ^. setFrom (each . _a)

-- |
-- __TODO: move this definition into a separate utility module.__
setFrom :: Getting (Set.Set a) s a -> Getter s (Set.Set a)
setFrom = to . Set.setOf
{-# INLINABLE setFrom #-}
