{-# LANGUAGE RankNTypes #-}
module Minicute.Transpiler.FreeVariables
  ( ProgramLWithFreeVariables
  , ExpressionLWithFreeVariables
  , FreeVariables
  , formFreeVariablesMainL
  , formFreeVariablesL
  ) where

import Control.Lens.Each
import Control.Lens.Getter ( Getting, to )
import Control.Lens.Iso ( coerced )
import Control.Lens.Operators
import Control.Lens.Type
import Control.Monad.Reader
import Minicute.Data.Fix
import Minicute.Types.Minicute.Program

import qualified Data.Set as Set
import qualified Data.Set.Lens as Set

type ProgramLWithFreeVariables a = AnnotatedProgramL FreeVariables a
type ExpressionLWithFreeVariables a = AnnotatedExpressionL FreeVariables a

type ExpressionWithFreeVariables_ = AnnotatedExpression_ FreeVariables Expression_
type ExpressionLWithFreeVariables_ = AnnotatedExpression_ FreeVariables ExpressionL_

-- |
-- Set of identifiers those are free variables of
-- annotated expression
type FreeVariables = Set.Set Identifier

formFreeVariablesMainL :: MainProgramL -> ProgramLWithFreeVariables Identifier
formFreeVariablesMainL = formFreeVariablesL id
{-# INLINEABLE formFreeVariablesMainL #-}

formFreeVariablesL :: Getter a Identifier -> ProgramL a -> ProgramLWithFreeVariables a
formFreeVariablesL _a
  = _supercombinators . each %~ formFreeVariablesSc
    where
      formFreeVariablesSc (binder, args, body)
        = (binder, args, runReader (formFVsEL _a body) $ args ^. setFrom (each . _a))

      {-# INLINEABLE formFreeVariablesSc #-}
{-# INLINEABLE formFreeVariablesL #-}

-- |
-- Set of identifiers those are candidates of free variables
type FVELEnvironment = Set.Set Identifier

type FVFormer e e' = e -> Reader FVELEnvironment e'

formFVsEL :: Getter a Identifier -> FVFormer (ExpressionL a) (ExpressionLWithFreeVariables a)
formFVsEL _a = coerced %~ formFVsEL_ _a _fv (formFVsEL _a)
  where
    _fv :: Lens' (AnnotatedExpressionL FreeVariables a) FreeVariables
    _fv = coerced . (_annotation :: Lens' (Fix2' ExpressionLWithFreeVariables_ a) FreeVariables)

formFVsEL_ :: Getter a Identifier -> Getter (aExpr_ a) FreeVariables -> FVFormer (expr_ a) (aExpr_ a) -> FVFormer (ExpressionL_ expr_ a) (ExpressionLWithFreeVariables_ aExpr_ a)
formFVsEL_ _a _fv fExpr (ELExpression_ expr_) = liftAnnExpr <$> formFVsE_ _a _fv fExpr expr_
  where
    liftAnnExpr (AnnotatedExpression_ (ann, aExpr'))
      = AnnotatedExpression_ (ann, ELExpression_ aExpr')
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
-- TODO - move this definition into a separate utility module.
-- TODO - add an hlint rule from 'Set.fromList $ a ^.. b' to 'a ^. setFrom b'
setFrom :: Getting (Set.Set a) s a -> Getter s (Set.Set a)
setFrom = to . Set.setOf
{-# INLINABLE setFrom #-}
