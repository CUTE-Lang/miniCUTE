module Minicute.Transpiler.FreeVariables
  ( ProgramLWithFreeVariable
  , ExpressionLWithFreeVariable
  , FreeVariables
  , formFreeVariablesL
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
  = AELApplication (Set.union (getAnnotationL expr1') (getAnnotationL expr2')) expr1' expr2'
  where
    expr2' = formFVsEL env expr2
    expr1' = formFVsEL env expr1
formFVsEL env (ELLet flag letDefs bodyExpr)
  = AELLet fvs flag letDefs' bodyExpr'
  where
    fvs = Set.union fvsInLetDefs' fvsInBodyExpr'
    fvsInBodyExpr' = Set.difference (getAnnotationL bodyExpr') letBinderSet
    fvsInLetDefs'
      | isRecursive flag = Set.difference fvsInLetDefBodies' letBinderSet
      | otherwise = fvsInLetDefBodies'
    fvsInLetDefBodies' = Set.unions (getAnnotationL <$> letDefBodies')

    letDefs' = zip letBinders letDefBodies'
    letDefBodies' = formFVsEL letDefEnv . getLetDefinitionBody <$> letDefs
    bodyExpr' = formFVsEL bodyEnv bodyExpr

    letDefEnv
      | isRecursive flag = bodyEnv
      | otherwise = env
    bodyEnv = letBinderSet <> env

    letBinderSet = Set.fromList letBinders
    letBinders = fmap getLetDefinitionBinder letDefs
formFVsEL env (ELMatch expr matchCases)
  = AELMatch fvs expr' matchCases'
  where
    fvs = Set.union (Set.unions fvssInMatchCases') fvsInExpr'
    fvssInMatchCases' = uncurry Set.difference <$> zip fvssInMatchCaseBodies' matchCaseArgumentSets
    fvssInMatchCaseBodies' = fmap getAnnotationL matchCasesBody'
    fvsInExpr' = getAnnotationL expr'

    matchCases' = uncurry (set _3) <$> zip matchCasesBody' matchCases
    matchCasesBody' = (\(args, body) -> formFVsEL (Set.union env args) body) <$> zip matchCaseArgumentSets matchCaseBodies
    expr' = formFVsEL env expr

    matchCaseBodies = getMatchCaseBody <$> matchCases
    matchCaseArgumentSets = Set.fromList . getMatchCaseArguments <$> matchCases
formFVsEL env (ELLambda args bodyExpr)
  = AELLambda fvs args bodyExpr'
  where
    fvs = Set.difference fvsInBodyExpr argSet
    fvsInBodyExpr = getAnnotationL bodyExpr'

    bodyExpr' = formFVsEL (argSet <> env) bodyExpr

    argSet = Set.fromList args

