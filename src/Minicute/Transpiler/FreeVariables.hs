module Minicute.Transpiler.FreeVariables
  ( ProgramLWithFreeVariable
  , ExpressionLWithFreeVariable
  , FreeVariables
  , getFreeVariablesL
  ) where

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

getFreeVariablesL :: MainProgramL -> ProgramLWithFreeVariable Identifier
getFreeVariablesL (ProgramL scs)
  = AnnotatedProgramL
    [ (binder, args, getFVsEL (Set.fromList args) body)
    | (binder, args, body) <- scs
    ]

-- |
-- Set of identifiers those are candidates of free variables
type FVELEnvironment = Set.Set Identifier

getFVsEL :: FVELEnvironment -> MainExpressionL -> ExpressionLWithFreeVariable Identifier
getFVsEL _ (ELInteger n) = AELInteger Set.empty n
getFVsEL _ (ELConstructor tag arity) = AELConstructor Set.empty tag arity
getFVsEL env (ELVariable v)
  | Set.member v env = AELVariable (Set.singleton v) v
  | otherwise = AELVariable Set.empty v
getFVsEL env (ELApplication expr1 expr2)
  = AELApplication (Set.union (getAnnotationL expr1') (getAnnotationL expr2')) expr1' expr2'
  where
    expr2' = getFVsEL env expr2
    expr1' = getFVsEL env expr1
getFVsEL env (ELLet flag letDefs bodyExpr)
  = AELLet fvs flag letDefs' bodyExpr'
  where
    fvs = Set.union fvsInLetDefs' fvsInBodyExpr'
    fvsInBodyExpr' = Set.difference (getAnnotationL bodyExpr') letBinderSet
    fvsInLetDefs'
      | isRecursive flag = Set.difference fvsInLetDefBodies' letBinderSet
      | otherwise = fvsInLetDefBodies'
    fvsInLetDefBodies' = Set.unions (getAnnotationL <$> letDefBodies')

    letDefs' = zip letBinders letDefBodies'
    letDefBodies' = getFVsEL letDefEnv . getLetDefinitionBody <$> letDefs
    bodyExpr' = getFVsEL bodyEnv bodyExpr

    letDefEnv
      | isRecursive flag = bodyEnv
      | otherwise = env
    bodyEnv = letBinderSet <> env

    letBinderSet = Set.fromList letBinders
    letBinders = fmap getLetDefinitionBinder letDefs
getFVsEL env (ELMatch expr matchCases)
  = AELMatch fvs expr' matchCases'
  where
    fvs = Set.union (Set.unions fvssInMatchCases') fvsInExpr'
    fvssInMatchCases' = uncurry Set.difference <$> zip fvssInMatchCaseBodies' matchCaseArgumentSets
    fvssInMatchCaseBodies' = fmap getAnnotationL matchCasesBody'
    fvsInExpr' = getAnnotationL expr'

    matchCases' = uncurry (set _3) <$> zip matchCasesBody' matchCases
    matchCasesBody' = (\(args, body) -> getFVsEL (Set.union env args) body) <$> zip matchCaseArgumentSets matchCaseBodies
    expr' = getFVsEL env expr

    matchCaseBodies = getMatchCaseBody <$> matchCases
    matchCaseArgumentSets = Set.fromList . getMatchCaseArguments <$> matchCases
getFVsEL env (ELLambda args bodyExpr)
  = AELLambda fvs args bodyExpr'
  where
    fvs = Set.difference fvsInBodyExpr argSet
    fvsInBodyExpr = getAnnotationL bodyExpr'

    bodyExpr' = getFVsEL (argSet <> env) bodyExpr

    argSet = Set.fromList args

