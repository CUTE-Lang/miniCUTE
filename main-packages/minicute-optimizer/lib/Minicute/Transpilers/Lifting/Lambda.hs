{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Copyright: (c) 2018-present Junyoung Clare Jang
-- License: BSD 3-Clause
--
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
import Minicute.Data.Minicute.Program
import Minicute.Transpilers.FreeVariables
import Minicute.Transpilers.VariablesRenaming

import qualified Data.Set as Set

-- |
-- Following function is based on the book
--
-- - Hughes, R. J. M. (__1983__) /Design and Implementation of Programming Languages/
--
-- For an alternative implementation, see
--
-- - Johnsson, T. (__1985__) /Lambda Lifting: Transforming Programs to Recursive Equations/
lambdaLifting :: MainProgram 'Simple 'MC -> MainProgram 'Simple 'LLMC
lambdaLifting = liftAnnons . renameVariablesMainMC . replaceLambda . formFreeVariablesMainMC

replaceLambda :: MainProgram ('AnnotatedWith FreeVariables) 'MC -> MainProgram 'Simple 'MC
replaceLambda = _Wrapped . each . _supercombinatorBody %~ replaceLambdaEMC

replaceLambdaEMC :: MainExpression ('AnnotatedWith FreeVariables) 'MC -> MainExpression 'Simple 'MC
replaceLambdaEMC (EInteger _ n) = EInteger () n
replaceLambdaEMC (EConstructor _ tag arity) = EConstructor () tag arity
replaceLambdaEMC (EVariable _ v) = EVariable () v
replaceLambdaEMC (EPrimitive _ prim) = EPrimitive () prim
replaceLambdaEMC (EApplication _ e1 e2) = EApplication () (replaceLambdaEMC e1) (replaceLambdaEMC e2)
replaceLambdaEMC (ELet _ flag lDefs expr) = ELet () flag (lDefs & each . _letDefinitionBody %~ replaceLambdaEMC) (replaceLambdaEMC expr)
replaceLambdaEMC (EMatch _ expr mCases) = EMatch () (replaceLambdaEMC expr) (mCases & each . _matchCaseBody %~ replaceLambdaEMC)
replaceLambdaEMC (ELambda fvs args expr) = foldl' (EApplication ()) annon (EVariable () <$> fvsList)
  where
    annon = ELet () NonRecursive [LetDefinition ("annon", annonBody)] (EVariable () "annon")
    annonBody = ELambda () (fvsList <> args) (replaceLambdaEMC expr)
    fvsList = Set.toList fvs

liftAnnons :: MainProgram 'Simple 'MC -> MainProgram 'Simple 'LLMC
liftAnnons = _Wrapped %~ concatMap liftAnnonsSc
  where
    liftAnnonsSc = uncurry (:) . swap . (_supercombinatorBody %%~ liftAnnonsEMC)

liftAnnonsEMC :: MainExpression 'Simple 'MC -> ([MainSupercombinator 'Simple 'LLMC], MainExpression 'Simple 'LLMC)
liftAnnonsEMC (EInteger _ n) = (mempty, EInteger () n)
liftAnnonsEMC (EConstructor _ tag arity) = (mempty, EConstructor () tag arity)
liftAnnonsEMC (EVariable _ v) = (mempty, EVariable () v)
liftAnnonsEMC (EPrimitive _ prim) = (mempty, EPrimitive () prim)
liftAnnonsEMC (EApplication _ e1 e2) = (scs1 <> scs2, EApplication () e1' e2')
  where
    (scs1, e1') = liftAnnonsEMC e1
    (scs2, e2') = liftAnnonsEMC e2
liftAnnonsEMC (ELet _ NonRecursive [LetDefinition (v1, ELambda () args e)] (EVariable () v2))
  | v1 == v2 = (pure (Supercombinator (v2, args, e')) <> scs, EVariable () v2)
  | otherwise = error "liftAnnonsEMC: wrong annonymous pattern is created"
  where
    (scs, e') = liftAnnonsEMC e
liftAnnonsEMC (ELet _ flag lDefs expr) = (lDefsScs <> exprScs, ELet () flag lDefs' expr')
  where
    (lDefsScs, lDefs') = lDefs & each . _letDefinitionBody %%~ liftAnnonsEMC
    (exprScs, expr') = liftAnnonsEMC expr
liftAnnonsEMC (EMatch _ expr mCases) = (exprScs <> mCasesScs, EMatch () expr' mCases')
  where
    (exprScs, expr') = liftAnnonsEMC expr
    (mCasesScs, mCases') = mCases & each . _matchCaseBody %%~ liftAnnonsEMC
liftAnnonsEMC (ELambda _ _ _) = error "liftAnnonsEMC: unexpected ELambda expression"
