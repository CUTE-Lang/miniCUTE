{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Transpilers to lift all lambda expressions as
-- supercombinators (top-level function definitions)
module Minicute.Transpilers.Lifting.Lambda
  ( lambdaLifting
  ) where

import Control.Lens.Each
import Control.Lens.Operators
import Control.Lens.Wrapped ( _Wrapped )
import Data.List
import Data.Tuple
import Minicute.Transpilers.FreeVariables
import Minicute.Transpilers.VariablesRenaming
import Minicute.Data.Minicute.Annotated.Program

import qualified Data.Set as Set

-- |
-- Following function is based on the book
--
-- - Hughes, R. J. M. (__1983__) /Design and Implementation of Programming Languages/
--
-- For an alternative implementation, see
--
-- - Johnsson, T. (__1985__) /Lambda Lifting: Transforming Programs to Recursive Equations/
lambdaLifting :: MainProgramL -> MainProgram
lambdaLifting = liftAnnons . renameVariablesMainL . replaceLambda . formFreeVariablesMainL

replaceLambda :: MainProgramLWithFreeVariables -> MainProgramL
replaceLambda = _Wrapped . each . _supercombinatorBody %~ replaceLambdaEL

replaceLambdaEL :: MainExpressionLWithFreeVariables -> MainExpressionL
replaceLambdaEL (AELInteger _ n) = EInteger n
replaceLambdaEL (AELConstructor _ tag arity) = EConstructor tag arity
replaceLambdaEL (AELVariable _ v) = EVariable v
replaceLambdaEL (AELApplication _ e1 e2) = EApplication (replaceLambdaEL e1) (replaceLambdaEL e2)
replaceLambdaEL (AELLet _ flag lDefs expr) = ELet flag (lDefs & each . _letDefinitionBody %~ replaceLambdaEL) (replaceLambdaEL expr)
replaceLambdaEL (AELMatch _ expr mCases) = EMatch (replaceLambdaEL expr) (mCases & each . _matchCaseBody %~ replaceLambdaEL)
replaceLambdaEL (AELLambda fvs args expr) = foldl' EApplication annon (EVariable <$> fvsList)
  where
    annon = ELet NonRecursive [LetDefinition ("annon", annonBody)] (EVariable "annon")
    annonBody = ELambda (fvsList <> args) (replaceLambdaEL expr)
    fvsList = Set.toList fvs

liftAnnons :: MainProgramL -> MainProgram
liftAnnons = _Wrapped %~ concatMap liftAnnonsSc
  where
    liftAnnonsSc = uncurry (:) . swap . (_supercombinatorBody %%~ liftAnnonsEL)

liftAnnonsEL :: MainExpressionL -> ([MainSupercombinator], MainExpression)
liftAnnonsEL (EInteger n) = (mempty, EInteger n)
liftAnnonsEL (EConstructor tag arity) = (mempty, EConstructor tag arity)
liftAnnonsEL (EVariable v) = (mempty, EVariable v)
liftAnnonsEL (EApplication e1 e2) = (scs1 <> scs2, EApplication e1' e2')
  where
    (scs1, e1') = liftAnnonsEL e1
    (scs2, e2') = liftAnnonsEL e2
liftAnnonsEL (ELet NonRecursive [LetDefinition (v1, ELambda args e)] (EVariable v2))
  | v1 == v2 = (pure (Supercombinator (v2, args, e')) <> scs, EVariable v2)
  | otherwise = error "liftAnnonsEL: wrong annonymous pattern is created"
  where
    (scs, e') = liftAnnonsEL e
liftAnnonsEL (ELet flag lDefs expr) = (lDefsScs <> exprScs, ELet flag lDefs' expr')
  where
    (lDefsScs, lDefs') = lDefs & each . _letDefinitionBody %%~ liftAnnonsEL
    (exprScs, expr') = liftAnnonsEL expr
liftAnnonsEL (EMatch expr mCases) = (exprScs <> mCasesScs, EMatch expr' mCases')
  where
    (exprScs, expr') = liftAnnonsEL expr
    (mCasesScs, mCases') = mCases & each . _matchCaseBody %%~ liftAnnonsEL
liftAnnonsEL (ELambda _ _) = error "liftAnnonsEL: unexpected ELambda expression"
