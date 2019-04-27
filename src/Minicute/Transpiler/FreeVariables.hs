{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
module Minicute.Transpiler.FreeVariables
  ( ProgramLWithFreeVariables
  , ExpressionLWithFreeVariables
  , FreeVariables
  , formFreeVariablesMainL
  , formFreeVariablesL
  ) where

import Control.Lens
import Control.Monad.Reader
import Minicute.Data.Fix
import Minicute.Types.Minicute.Program

import qualified Data.Set as Set

type ProgramLWithFreeVariables a = AnnotatedProgramL FreeVariables a
type ExpressionLWithFreeVariables a = AnnotatedExpressionL FreeVariables a

-- |
-- Set of identifiers those are free variables of
-- annotated expression
type FreeVariables = Set.Set Identifier

formFreeVariablesMainL :: MainProgramL -> ProgramLWithFreeVariables Identifier
formFreeVariablesMainL = formFreeVariablesL id
{-# INLINEABLE formFreeVariablesMainL #-}

formFreeVariablesL :: Getter a Identifier -> ProgramL a -> ProgramLWithFreeVariables a
formFreeVariablesL fA
  = over _supercombinators (fmap formFreeVariablesSc)
    where
      formFreeVariablesSc (binder, args, body)
        = (binder, args, runReader (formFVsEL fA body) (Set.fromList (view fA <$> args)))

      {-# INLINEABLE formFreeVariablesSc #-}
{-# INLINEABLE formFreeVariablesL #-}

-- |
-- Set of identifiers those are candidates of free variables
type FVELEnvironment = Set.Set Identifier

type FVFormer e e' = e -> Reader FVELEnvironment e'

formFVsEL :: Getter a Identifier -> FVFormer (ExpressionL a) (ExpressionLWithFreeVariables a)
formFVsEL _a = over coerced (formFVsEL# _a _fv (formFVsEL _a))
  where
    _fv :: Lens' (AnnotatedExpressionL FreeVariables a) FreeVariables
    _fv = coerced . (_annotationL :: Lens' (Fix2' (AnnotatedExpressionL# FreeVariables) a) FreeVariables)

formFVsEL# :: Getter a Identifier -> Getter (aExpr_ a) FreeVariables -> FVFormer (expr_ a) (aExpr_ a) -> FVFormer (ExpressionL# expr_ a) (AnnotatedExpressionL# FreeVariables aExpr_ a)
formFVsEL# _a _fv fExpr (ELExpression# expr#) = liftAnnExpr <$> formFVsE# _a _fv fExpr expr#
  where
    liftAnnExpr (AnnotatedExpression# (ann, aExpr'))
      = AnnotatedExpressionL# (ann, ELExpression# aExpr')
formFVsEL# _a _fv fExpr (ELLambda# args expr) = do
  expr' <- local (argIdSet <>) . fExpr $ expr

  let
    fvsExpr' = view _fv expr'
    fvs = fvsExpr' Set.\\ argIdSet

    {-# INLINEABLE fvsExpr' #-}
    {-# INLINEABLE fvs #-}
  return (AnnotatedExpressionL# (fvs, ELLambda# args expr'))
  where
    argIdSet = Set.fromList (view _a <$> args)

formFVsE# :: Getter a Identifier -> Getter (aExpr_ a) FreeVariables -> FVFormer (expr_ a) (aExpr_ a) -> FVFormer (Expression# expr_ a) (AnnotatedExpression# FreeVariables aExpr_ a)
formFVsE# _ _ _ (EInteger# n) = return (AnnotatedExpression# (Set.empty, EInteger# n))
formFVsE# _ _ _ (EConstructor# tag arity) = return (AnnotatedExpression# (Set.empty, EConstructor# tag arity))
formFVsE# _ _ _ (EVariable# v) = do
  env <- ask

  let
    fvs
      | Set.member v env = Set.singleton v
      | otherwise = Set.empty

    {-# INLINEABLE fvs #-}
  return (AnnotatedExpression# (fvs, EVariable# v))
formFVsE# _ _fv fExpr (EApplication# expr1 expr2) = do
  expr1' <- fExpr expr1
  expr2' <- fExpr expr2

  let
    fvs = view _fv expr1' <> view _fv expr2'

    {-# INLINEABLE fvs #-}
  return (AnnotatedExpression# (fvs, EApplication# expr1' expr2'))
formFVsE# _a _fv fExpr (ELet# flag lDefs expr) = do
  env <- ask

  let
    exprEnv = lDefsBinderIdSet <> env
    lDefsEnv
      | isRecursive flag = exprEnv
      | otherwise = env

    formLDefsBodies = traverse (traverseOf _letDefinitionBody fExpr)

    {-# INLINEABLE lDefsEnv #-}
    {-# INLINEABLE formLDefsBodies #-}
  expr' <- local (const exprEnv) . fExpr $ expr
  lDefs' <- local (const lDefsEnv) . formLDefsBodies $ lDefs

  let
    fvsLDefsBodies' = mconcat (lDefs' <&> view (_letDefinitionBody . _fv))
    fvsLDefs'
      | isRecursive flag = fvsLDefsBodies' Set.\\ lDefsBinderIdSet
      | otherwise = fvsLDefsBodies'
    fvsExpr' = view _fv expr' Set.\\ lDefsBinderIdSet
    fvs = fvsLDefs' <> fvsExpr'

    {-# INLINEABLE fvsLDefsBodies' #-}
    {-# INLINEABLE fvsLDefs' #-}
    {-# INLINEABLE fvsExpr' #-}
    {-# INLINEABLE fvs #-}
  return (AnnotatedExpression# (fvs, ELet# flag lDefs' expr'))
  where
    lDefsBinderIdSet = Set.fromList (lDefs <&> view (_letDefinitionBinder . _a))
formFVsE# _a _fv fExpr (EMatch# expr mCases) = do
  let
    formMCaseBodies mCaseArgSet = local (mCaseArgSet <>) . fExpr
    formMCase mCaseArgSet = traverseOf _matchCaseBody (formMCaseBodies mCaseArgSet)

    {-# INLINEABLE formMCaseBodies #-}
    {-# INLINEABLE formMCase #-}
  expr' <- fExpr expr
  mCases' <- zipWithM formMCase mCasesArgumentSets mCases

  let
    fvssMCasesBodies' = mCases' <&> view (_matchCaseBody . _fv)
    fvsMCases' = mconcat (zipWith (Set.\\) fvssMCasesBodies' mCasesArgumentSets)
    fvs = fvsMCases' <> view _fv expr'

    {-# INLINEABLE fvssMCasesBodies' #-}
    {-# INLINEABLE fvsMCases' #-}
    {-# INLINEABLE fvs #-}
  return (AnnotatedExpression# (fvs, EMatch# expr' mCases'))
  where
    mCasesArgumentSets = Set.fromList . (view _a <$>) . view _matchCaseArguments <$> mCases
