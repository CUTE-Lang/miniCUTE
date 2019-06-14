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
import Minicute.Types.Minicute.Annotated.Program

import qualified Data.Set as Set
import qualified Data.Set.Lens as Set

-- |
-- 'ProgramL' annotated with 'FreeVariables'
type ProgramLWithFreeVariables = AnnotatedProgramL FreeVariables
-- |
-- 'MainProgramL' annotated with 'FreeVariables'
type MainProgramLWithFreeVariables = MainAnnotatedProgramL FreeVariables
-- |
-- 'ExpressionL' annotated with 'FreeVariables'
type ExpressionLWithFreeVariables = AnnotatedExpressionL FreeVariables
-- |
-- 'MainExpressionL' annotated with 'FreeVariables'
type MainExpressionLWithFreeVariables = MainAnnotatedExpressionL FreeVariables

type ExpressionWithFreeVariables_ = AnnotatedExpression_ FreeVariables Expression_
type ExpressionLWithFreeVariables_ = AnnotatedExpression_ FreeVariables ExpressionL_

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
formFreeVariablesL :: Getter a Identifier -> ProgramL a -> ProgramLWithFreeVariables a
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

formFVsEL :: Getter a Identifier -> FVFormer (ExpressionL a) (ExpressionLWithFreeVariables a)
formFVsEL _a = _Wrapped %%~ formFVsEL_ _a (_Wrapped . _annotation) (formFVsEL _a)

formFVsEL_ :: Getter a Identifier -> Getter (aExpr_ a) FreeVariables -> FVFormer (expr_ a) (aExpr_ a) -> FVFormer (ExpressionL_ expr_ a) (ExpressionLWithFreeVariables_ aExpr_ a)
formFVsEL_ _a _fv fExpr (ELExpression_ expr_)
  = formFVsE_ _a _fv fExpr expr_ <&> _annotated %~ ELExpression_
formFVsEL_ _a _fv fExpr (ELLambda_ args expr) = do
  expr' <- local (argIdSet <>) . fExpr $ expr

  let
    fvsExpr' = expr' ^. _fv
    fvs = fvsExpr' Set.\\ argIdSet

    {-# INLINEABLE fvsExpr' #-}
    {-# INLINEABLE fvs #-}
  return (AnnotatedExpression_ (fvs, ELLambda_ args expr'))
  where
    argIdSet = args ^. setFrom (each . _a)

formFVsE_ :: Getter a Identifier -> Getter (aExpr_ a) FreeVariables -> FVFormer (expr_ a) (aExpr_ a) -> FVFormer (Expression_ expr_ a) (ExpressionWithFreeVariables_ aExpr_ a)
formFVsE_ _ _ _ (EInteger_ n) = return (AnnotatedExpression_ (Set.empty, EInteger_ n))
formFVsE_ _ _ _ (EConstructor_ tag arity) = return (AnnotatedExpression_ (Set.empty, EConstructor_ tag arity))
formFVsE_ _ _ _ (EVariable_ v) = do
  env <- ask

  let
    fvs
      | Set.member v env = Set.singleton v
      | otherwise = Set.empty

    {-# INLINEABLE fvs #-}
  return (AnnotatedExpression_ (fvs, EVariable_ v))
formFVsE_ _ _fv fExpr (EApplication_ expr1 expr2) = do
  expr1' <- fExpr expr1
  expr2' <- fExpr expr2

  let
    fvs = expr1' ^. _fv <> expr2' ^. _fv

    {-# INLINEABLE fvs #-}
  return (AnnotatedExpression_ (fvs, EApplication_ expr1' expr2'))
formFVsE_ _a _fv fExpr (ELet_ flag lDefs expr) = do
  env <- ask

  let
    exprEnv = lDefsBinderIdSet <> env
    lDefsEnv
      | isRecursive flag = exprEnv
      | otherwise = env

    formLDefsBodies = each . _letDefinitionBody %%~ fExpr

    {-# INLINEABLE lDefsEnv #-}
    {-# INLINEABLE formLDefsBodies #-}
  expr' <- local (const exprEnv) . fExpr $ expr
  lDefs' <- local (const lDefsEnv) . formLDefsBodies $ lDefs

  let
    fvsLDefsBodies' = lDefs' ^. each . _letDefinitionBody . _fv
    fvsLDefs'
      | isRecursive flag = fvsLDefsBodies' Set.\\ lDefsBinderIdSet
      | otherwise = fvsLDefsBodies'
    fvsExpr' = (expr' ^. _fv) Set.\\ lDefsBinderIdSet
    fvs = fvsLDefs' <> fvsExpr'

    {-# INLINEABLE fvsLDefsBodies' #-}
    {-# INLINEABLE fvsLDefs' #-}
    {-# INLINEABLE fvsExpr' #-}
    {-# INLINEABLE fvs #-}
  return (AnnotatedExpression_ (fvs, ELet_ flag lDefs' expr'))
  where
    lDefsBinderIdSet = lDefs ^. setFrom (each . _letDefinitionBinder . _a)
formFVsE_ _a _fv fExpr (EMatch_ expr mCases) = do
  let
    formMCaseBodies mCaseArgSet = local (mCaseArgSet <>) . fExpr
    formMCase mCaseArgSet = _matchCaseBody %%~ formMCaseBodies mCaseArgSet

    {-# INLINEABLE formMCaseBodies #-}
    {-# INLINEABLE formMCase #-}
  expr' <- fExpr expr
  mCases' <- zipWithM formMCase mCasesArgumentSets mCases

  let
    fvssMCasesBodies' = mCases' ^.. each . _matchCaseBody . _fv
    fvsMCases' = mconcat (zipWith (Set.\\) fvssMCasesBodies' mCasesArgumentSets)
    fvs = fvsMCases' <> expr' ^. _fv

    {-# INLINEABLE fvssMCasesBodies' #-}
    {-# INLINEABLE fvsMCases' #-}
    {-# INLINEABLE fvs #-}
  return (AnnotatedExpression_ (fvs, EMatch_ expr' mCases'))
  where
    mCasesArgumentSets = mCases ^.. each . _matchCaseArguments . setFrom (each . _a)

-- |
-- __TODO: move this definition into a separate utility module.__
--
-- __TODO: add an hlint rule from 'Set.fromList $ a ^.. b' to 'a ^. setFrom b'__
setFrom :: Getting (Set.Set a) s a -> Getter s (Set.Set a)
setFrom = to . Set.setOf
{-# INLINABLE setFrom #-}
