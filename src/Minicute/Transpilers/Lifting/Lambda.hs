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
lambdaLifting :: MainProgramMC -> MainProgramLLMC
lambdaLifting = liftAnnons . renameVariablesMainMC . replaceLambda . formFreeVariablesMainMC

replaceLambda :: MainProgramMCWithFreeVariables -> MainProgramMC
replaceLambda = _Wrapped . each . _supercombinatorBody %~ replaceLambdaEMC

replaceLambdaEMC :: MainExpressionMCWithFreeVariables -> MainExpressionMC
replaceLambdaEMC (AEInteger _ n) = EInteger n
replaceLambdaEMC (AEConstructor _ tag arity) = EConstructor tag arity
replaceLambdaEMC (AEVariable _ v) = EVariable v
replaceLambdaEMC (AEApplication _ e1 e2) = EApplication (replaceLambdaEMC e1) (replaceLambdaEMC e2)
replaceLambdaEMC (AELet _ flag lDefs expr) = ELet flag (lDefs & each . _letDefinitionBody %~ replaceLambdaEMC) (replaceLambdaEMC expr)
replaceLambdaEMC (AEMatch _ expr mCases) = EMatch (replaceLambdaEMC expr) (mCases & each . _matchCaseBody %~ replaceLambdaEMC)
replaceLambdaEMC (AELambda fvs args expr) = foldl' EApplication annon (EVariable <$> fvsList)
  where
    annon = ELet NonRecursive [LetDefinition ("annon", annonBody)] (EVariable "annon")
    annonBody = ELambda (fvsList <> args) (replaceLambdaEMC expr)
    fvsList = Set.toList fvs

liftAnnons :: MainProgramMC -> MainProgramLLMC
liftAnnons = _Wrapped %~ concatMap liftAnnonsSc
  where
    liftAnnonsSc = uncurry (:) . swap . (_supercombinatorBody %%~ liftAnnonsEMC)

liftAnnonsEMC :: MainExpressionMC -> ([MainSupercombinatorLLMC], MainExpressionLLMC)
liftAnnonsEMC (EInteger n) = (mempty, EInteger n)
liftAnnonsEMC (EConstructor tag arity) = (mempty, EConstructor tag arity)
liftAnnonsEMC (EVariable v) = (mempty, EVariable v)
liftAnnonsEMC (EApplication e1 e2) = (scs1 <> scs2, EApplication e1' e2')
  where
    (scs1, e1') = liftAnnonsEMC e1
    (scs2, e2') = liftAnnonsEMC e2
liftAnnonsEMC (ELet NonRecursive [LetDefinition (v1, ELambda args e)] (EVariable v2))
  | v1 == v2 = (pure (Supercombinator (v2, args, e')) <> scs, EVariable v2)
  | otherwise = error "liftAnnonsEMC: wrong annonymous pattern is created"
  where
    (scs, e') = liftAnnonsEMC e
liftAnnonsEMC (ELet flag lDefs expr) = (lDefsScs <> exprScs, ELet flag lDefs' expr')
  where
    (lDefsScs, lDefs') = lDefs & each . _letDefinitionBody %%~ liftAnnonsEMC
    (exprScs, expr') = liftAnnonsEMC expr
liftAnnonsEMC (EMatch expr mCases) = (exprScs <> mCasesScs, EMatch expr' mCases')
  where
    (exprScs, expr') = liftAnnonsEMC expr
    (mCasesScs, mCases') = mCases & each . _matchCaseBody %%~ liftAnnonsEMC
liftAnnonsEMC (ELambda _ _) = error "liftAnnonsEMC: unexpected ELambda expression"
