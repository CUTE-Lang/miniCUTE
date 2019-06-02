{-# LANGUAGE OverloadedStrings #-}
module Minicute.Transpilers.LambdaLifting where

import Control.Lens.Each
import Control.Lens.Operators
import Data.List
import Data.Tuple
import Minicute.Transpilers.FreeVariables
import Minicute.Transpilers.VariablesRenaming
import Minicute.Types.Minicute.Annotated.Program

import qualified Data.Set as Set

-- |
-- Following function is based on the book
-- "Hughes, R. J. M. (1983) Design and Implementation of Programming Languages"
-- For an alternative implementation, see
-- "Johnsson, T. (1985) Lambda Lifting: Transforming Programs to Recursive Equations"
lambdaLifting :: MainProgramL -> MainProgram
lambdaLifting = liftAnnons . renameVariablesMainL . replaceLambda . formFreeVariablesMainL

replaceLambda :: MainProgramLWithFreeVariables -> MainProgramL
replaceLambda = _supercombinators . each . _supercombinatorBody %~ replaceLambdaEL

replaceLambdaEL :: MainExpressionLWithFreeVariables -> MainExpressionL
replaceLambdaEL (AELInteger _ n) = ELInteger n
replaceLambdaEL (AELConstructor _ tag arity) = ELConstructor tag arity
replaceLambdaEL (AELVariable _ v) = ELVariable v
replaceLambdaEL (AELApplication _ e1 e2) = ELApplication (replaceLambdaEL e1) (replaceLambdaEL e2)
replaceLambdaEL (AELLet _ flag lDefs expr) = ELLet flag (lDefs & each . _letDefinitionBody %~ replaceLambdaEL) (replaceLambdaEL expr)
replaceLambdaEL (AELMatch _ expr mCases) = ELMatch (replaceLambdaEL expr) (mCases & each . _matchCaseBody %~ replaceLambdaEL)
replaceLambdaEL (AELLambda fvs args expr) = foldl' ELApplication annon (ELVariable <$> fvsList)
  where
    annon = ELLet NonRecursive [LetDefinitionL "annon" annonBody] (ELVariable "annon")
    annonBody = ELLambda (fvsList <> args) (replaceLambdaEL expr)
    fvsList = Set.toList fvs

liftAnnons :: MainProgramL -> MainProgram
liftAnnons = _supercombinators %~ concatMap liftAnnonsSc
  where
    liftAnnonsSc = uncurry (:) . swap . (_supercombinatorBody %%~ liftAnnonsEL)

liftAnnonsEL :: MainExpressionL -> ([MainSupercombinator], MainExpression)
liftAnnonsEL (ELInteger n) = (mempty, EInteger n)
liftAnnonsEL (ELConstructor tag arity) = (mempty, EConstructor tag arity)
liftAnnonsEL (ELVariable v) = (mempty, EVariable v)
liftAnnonsEL (ELApplication e1 e2) = (scs1 <> scs2, EApplication e1' e2')
  where
    (scs1, e1') = liftAnnonsEL e1
    (scs2, e2') = liftAnnonsEL e2
liftAnnonsEL (ELLet NonRecursive [LetDefinitionL v1 (ELLambda args e)] (ELVariable v2))
  | v1 == v2 = (pure (v2, args, e') <> scs, EVariable v2)
  | otherwise = error "liftAnnonsEL: wrong annonymous pattern is created"
  where
    (scs, e') = liftAnnonsEL e
liftAnnonsEL (ELLet flag lDefs expr) = (lDefsScs <> exprScs, ELet flag lDefs' expr')
  where
    (lDefsScs, lDefs') = lDefs & each . _letDefinitionBody %%~ liftAnnonsEL
    (exprScs, expr') = liftAnnonsEL expr
liftAnnonsEL (ELMatch expr mCases) = (exprScs <> mCasesScs, EMatch expr' mCases')
  where
    (exprScs, expr') = liftAnnonsEL expr
    (mCasesScs, mCases') = mCases & each . _matchCaseBody %%~ liftAnnonsEL
liftAnnonsEL (ELLambda _ _) = error "liftAnnonsEL: unexpected ELLambda expression"
