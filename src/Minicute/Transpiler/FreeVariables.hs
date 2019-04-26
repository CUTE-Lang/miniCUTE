module Minicute.Transpiler.FreeVariables
  ( ProgramLWithFreeVariables
  , ExpressionLWithFreeVariables
  , FreeVariables
  , formFreeVariablesMainL
  , formFreeVariablesL
  ) where

import Control.Lens.Getter
import Control.Lens.Setter
import Control.Lens.Tuple
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

formFreeVariablesL :: (a -> Identifier) -> ProgramL a -> ProgramLWithFreeVariables a
formFreeVariablesL fA
  = over _supercombinators (fmap formFreeVariablesSc)
    where
      formFreeVariablesSc (binder, args, body)
        = (binder, args, formFVsEL fA (Set.fromList (fA <$> args)) body)

      {-# INLINEABLE formFreeVariablesSc #-}
{-# INLINEABLE formFreeVariablesL #-}

-- |
-- Set of identifiers those are candidates of free variables
type FVELEnvironment = Set.Set Identifier

formFVsEL :: (a -> Identifier) -> FVELEnvironment -> ExpressionL a -> ExpressionLWithFreeVariables a
formFVsEL _ _ (ELInteger n) = AELInteger Set.empty n
formFVsEL _ _ (ELConstructor tag arity) = AELConstructor Set.empty tag arity
formFVsEL _ env (ELVariable v) = AELVariable fvs v
  where
    fvs
      | Set.member v env = Set.singleton v
      | otherwise = Set.empty

    {-# INLINEABLE fvs #-}
formFVsEL fA env (ELApplication expr1 expr2)
  = AELApplication (getFVOfE expr1' <> getFVOfE expr2') expr1' expr2'
  where
    expr2' = formFVsEL fA env expr2
    expr1' = formFVsEL fA env expr1
formFVsEL fA env (ELLet flag lDefs expr)
  = AELLet fvs flag lDefs' expr'
  where
    fvs = fvsInLDefs' <> fvsInExpr'
    fvsInExpr' = getFVOfE expr' Set.\\ lDefBinderIdentifierSet
    fvsInLDefs'
      | isRecursive flag = fvsInLDefBodies' Set.\\ lDefBinderIdentifierSet
      | otherwise = fvsInLDefBodies'
    fvsInLDefBodies' = mconcat (getFVOfE <$> lDefBodies')

    lDefs' = zip lDefBinders lDefBodies'
    lDefBodies' = formFVsEL fA lDefEnv . view _letDefinitionBody <$> lDefs
    expr' = formFVsEL fA env' expr

    env' = lDefBinderIdentifierSet <> env
    lDefEnv
      | isRecursive flag = env'
      | otherwise = env

    lDefBinderIdentifierSet = Set.fromList (fA <$> lDefBinders)
    lDefBinders = view _letDefinitionBinder <$> lDefs

    {-# INLINEABLE fvs #-}
    {-# INLINEABLE fvsInExpr' #-}
    {-# INLINEABLE fvsInLDefs' #-}
    {-# INLINEABLE fvsInLDefBodies' #-}
    {-# INLINEABLE lDefs' #-}
    {-# INLINEABLE lDefEnv #-}
formFVsEL fA env (ELMatch expr mCases)
  = AELMatch fvs expr' mCases'
  where
    fvs = fvsInMCases' <> getFVOfE expr'
    fvsInMCases' = mconcat (zipWith (Set.\\) fvssInMCaseBodies' mCaseArgumentSets)
    fvssInMCaseBodies' = getFVOfE <$> mCaseBodies'

    mCases' = zipWith (set _3) mCaseBodies' mCases
    mCaseBodies' = zipWith (formFVsEL fA) ((<> env) <$> mCaseArgumentSets) mCaseBodies
    expr' = formFVsEL fA env expr

    mCaseBodies = view _matchCaseBody <$> mCases
    mCaseArgumentSets = Set.fromList . (fA <$>) . view _matchCaseArguments <$> mCases

    {-# INLINEABLE fvs #-}
    {-# INLINEABLE fvsInMCases' #-}
    {-# INLINEABLE fvssInMCaseBodies' #-}
    {-# INLINEABLE mCases' #-}
    {-# INLINEABLE mCaseBodies #-}
formFVsEL fA env (ELLambda args expr)
  = AELLambda fvs args expr'
  where
    fvs = fvsInExpr' Set.\\ argIdentifierSet
    fvsInExpr' = getFVOfE expr'

    expr' = formFVsEL fA (argIdentifierSet <> env) expr

    argIdentifierSet = Set.fromList (fA <$> args)

    {-# INLINEABLE fvs #-}
    {-# INLINEABLE fvsInExpr' #-}

getFVOfE :: ExpressionLWithFreeVariables a -> FreeVariables
getFVOfE = view _annotationL
{-# INLINEABLE getFVOfE #-}
