module Minicute.Transpiler.FreeVariables
  ( ProgramLWithFreeVariable
  , ExpressionLWithFreeVariable
  , FreeVariables
  , formFreeVariablesL
  ) where

import Control.Lens.Getter
import Control.Lens.Setter
import Control.Lens.Tuple
import Minicute.Types.Minicute.Program

import qualified Data.Set as Set

type ProgramLWithFreeVariable a = AnnotatedProgramL FreeVariables a
type ExpressionLWithFreeVariable a = AnnotatedExpressionL FreeVariables a

-- |
-- Set of identifiers those are free variables of
-- annotated expression
type FreeVariables = Set.Set Identifier

formFreeVariablesL :: MainProgramL -> ProgramLWithFreeVariable Identifier
formFreeVariablesL (ProgramL scs)
  = AnnotatedProgramL
    [ (binder, args, formFVsEL (Set.fromList args) body)
    | (binder, args, body) <- scs
    ]

-- |
-- Set of identifiers those are candidates of free variables
type FVELEnvironment = Set.Set Identifier

formFVsEL :: FVELEnvironment -> MainExpressionL -> ExpressionLWithFreeVariable Identifier
formFVsEL _ (ELInteger n) = AELInteger Set.empty n
formFVsEL _ (ELConstructor tag arity) = AELConstructor Set.empty tag arity
formFVsEL env (ELVariable v) = AELVariable fvs v
  where
    fvs
      | Set.member v env = Set.singleton v
      | otherwise = Set.empty
formFVsEL env (ELApplication expr1 expr2)
  = AELApplication (getFVOfE expr1' <> getFVOfE expr2') expr1' expr2'
  where
    expr2' = formFVsEL env expr2
    expr1' = formFVsEL env expr1
formFVsEL env (ELLet flag lDefs expr)
  = AELLet fvs flag lDefs' expr'
  where
    fvs = fvsInLDefs' <> fvsInExpr'
    fvsInExpr' = getFVOfE expr' Set.\\ lDefBinderSet
    fvsInLDefs'
      | isRecursive flag = fvsInLDefBodies' Set.\\ lDefBinderSet
      | otherwise = fvsInLDefBodies'
    fvsInLDefBodies' = mconcat (getFVOfE <$> lDefBodies')

    lDefs' = zip lDefBinders lDefBodies'
    lDefBodies' = formFVsEL lDefEnv . view letDefinitionBody <$> lDefs
    expr' = formFVsEL env' expr

    env' = lDefBinderSet <> env
    lDefEnv
      | isRecursive flag = env'
      | otherwise = env

    lDefBinderSet = Set.fromList lDefBinders
    lDefBinders = view letDefinitionBinder <$> lDefs
formFVsEL env (ELMatch expr mCases)
  = AELMatch fvs expr' mCases'
  where
    fvs = mconcat fvssInMCases' <> getFVOfE expr'
    fvssInMCases' = zipWith (Set.\\) fvssInMCaseBodies' mCaseArgumentSets
    fvssInMCaseBodies' = getFVOfE <$> mCaseBodies'

    mCases' = zipWith (set _3) mCaseBodies' mCases
    mCaseBodies' = zipWith formFVsEL ((<> env) <$> mCaseArgumentSets) mCaseBodies
    expr' = formFVsEL env expr

    mCaseBodies = view matchCaseBody <$> mCases
    mCaseArgumentSets = Set.fromList . view matchCaseArguments <$> mCases
formFVsEL env (ELLambda args expr)
  = AELLambda fvs args expr'
  where
    fvs = fvsInExpr' Set.\\ argSet
    fvsInExpr' = getFVOfE expr'

    expr' = formFVsEL (argSet <> env) expr

    argSet = Set.fromList args

getFVOfE :: ExpressionLWithFreeVariable Identifier -> FreeVariables
getFVOfE = view annotationL
