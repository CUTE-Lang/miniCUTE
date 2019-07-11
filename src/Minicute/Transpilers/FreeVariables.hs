{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Transpilers to extract free variable information of expressions
module Minicute.Transpilers.FreeVariables
  ( ProgramLWithFreeVariables
  , MainProgramLWithFreeVariables
  , ExpressionLWithFreeVariables
  , MainExpressionLWithFreeVariables

  , FreeVariables

  , formFreeVariablesMainL
  , formFreeVariablesL
  ) where

import Control.Lens.Each
import Control.Lens.Getter ( Getting, to )
import Control.Lens.Operators
import Control.Lens.Type
import Control.Lens.Wrapped ( _Wrapped )
import Control.Monad.Reader
import Minicute.Data.Minicute.Annotated.Program

import qualified Data.Set as Set
import qualified Data.Set.Lens as Set

-- |
-- 'ProgramL' annotated with 'FreeVariables'
type ProgramLWithFreeVariables = Program (AnnotatedExpressionL FreeVariables)
-- |
-- 'MainProgramL' annotated with 'FreeVariables'
type MainProgramLWithFreeVariables = MainAnnotatedProgramL FreeVariables
-- |
-- 'ExpressionL' annotated with 'FreeVariables'
type ExpressionLWithFreeVariables = AnnotatedExpressionL FreeVariables
-- |
-- 'MainExpressionL' annotated with 'FreeVariables'
type MainExpressionLWithFreeVariables = MainAnnotatedExpressionL FreeVariables

-- |
-- A set of identifiers that are free in the annotated expression
type FreeVariables = Set.Set Identifier

-- |
-- A transpiler to create free variable information for 'MainProgramL'
formFreeVariablesMainL :: MainProgramL -> MainProgramLWithFreeVariables
formFreeVariablesMainL = formFreeVariablesL id
{-# INLINEABLE formFreeVariablesMainL #-}

-- |
-- A transpiler to create free variable information for 'ProgramL'
formFreeVariablesL :: Getter a Identifier -> Program ExpressionL a -> ProgramLWithFreeVariables a
formFreeVariablesL _a
  = _Wrapped . each %~ formFreeVariablesSc
    where
      formFreeVariablesSc sc
        = sc & _supercombinatorBody %~ flip runReader scArgsSet . formFVsEL _a
        where
          scArgsSet = sc ^. _supercombinatorArguments . setFrom (each . _a)

      {-# INLINEABLE formFreeVariablesSc #-}
{-# INLINEABLE formFreeVariablesL #-}

-- |
-- Set of identifiers those are candidates of free variables
type FVELEnvironment = Set.Set Identifier

type FVFormer e e' = e -> Reader FVELEnvironment e'

-- formFVsEL :: Getter a Identifier -> FVFormer (ExpressionL a) (ExpressionLWithFreeVariables a)
-- formFVsEL _a = _Wrapped %%~ formFVsEL _a (_Wrapped . _annotation) (formFVsEL _a)

formFVsEL :: Getter a Identifier -> FVFormer (ExpressionL a) (ExpressionLWithFreeVariables a)
formFVsEL _ (ELInteger n) = return (AELInteger Set.empty n)
formFVsEL _ (ELConstructor tag arity) = return (AELConstructor Set.empty tag arity)
formFVsEL _ (ELVariable v) = do
  env <- ask

  let
    fvs
      | Set.member v env = Set.singleton v
      | otherwise = Set.empty

    {-# INLINEABLE fvs #-}
  return (AELVariable fvs v)
formFVsEL _a (ELApplication expr1 expr2) = do
  expr1' <- formFVsEL _a expr1
  expr2' <- formFVsEL _a expr2

  let
    fvs = expr1' ^. _annotation <> expr2' ^. _annotation

    {-# INLINEABLE fvs #-}
  return (AELApplication fvs expr1' expr2')
formFVsEL _a (ELLet flag lDefs expr) = do
  env <- ask

  let
    exprEnv = lDefsBinderIdSet <> env
    lDefsEnv
      | isRecursive flag = exprEnv
      | otherwise = env

    formLDefsBodies = each . _letDefinitionBody %%~ formFVsEL _a

    {-# INLINEABLE lDefsEnv #-}
    {-# INLINEABLE formLDefsBodies #-}
  expr' <- local (const exprEnv) . formFVsEL _a $ expr
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
  return (AELLet fvs flag lDefs' expr')
  where
    lDefsBinderIdSet = lDefs ^. setFrom (each . _letDefinitionBinder . _a)
formFVsEL _a (ELMatch expr mCases) = do
  let
    formMCaseBodies mCaseArgSet = local (mCaseArgSet <>) . formFVsEL _a
    formMCase mCaseArgSet = _matchCaseBody %%~ formMCaseBodies mCaseArgSet

    {-# INLINEABLE formMCaseBodies #-}
    {-# INLINEABLE formMCase #-}
  expr' <- formFVsEL _a expr
  mCases' <- zipWithM formMCase mCasesArgumentSets mCases

  let
    fvssMCasesBodies' = mCases' ^.. each . _matchCaseBody . _annotation
    fvsMCases' = mconcat (zipWith (Set.\\) fvssMCasesBodies' mCasesArgumentSets)
    fvs = fvsMCases' <> expr' ^. _annotation

    {-# INLINEABLE fvssMCasesBodies' #-}
    {-# INLINEABLE fvsMCases' #-}
    {-# INLINEABLE fvs #-}
  return (AELMatch fvs expr' mCases')
  where
    mCasesArgumentSets = mCases ^.. each . _matchCaseArguments . setFrom (each . _a)
formFVsEL _a (ELLambda args expr) = do
  expr' <- local (argIdSet <>) . formFVsEL _a $ expr

  let
    fvsExpr' = expr' ^. _annotation
    fvs = fvsExpr' Set.\\ argIdSet

    {-# INLINEABLE fvsExpr' #-}
    {-# INLINEABLE fvs #-}
  return (AELLambda fvs args expr')
  where
    argIdSet = args ^. setFrom (each . _a)

-- |
-- __TODO: move this definition into a separate utility module.__
--
-- __TODO: add an hlint rule from 'Set.fromList $ a ^.. b' to 'a ^. setFrom b'__
setFrom :: Getting (Set.Set a) s a -> Getter s (Set.Set a)
setFrom = to . Set.setOf
{-# INLINABLE setFrom #-}
