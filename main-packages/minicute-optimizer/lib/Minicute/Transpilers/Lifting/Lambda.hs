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
  ( module Minicute.Data.Minicute.Program

  , lambdaLifting
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
lambdaLifting :: MainProgram t l -> MainProgram 'Simple 'LLMC
lambdaLifting
  = liftAnnons
    . renameVariablesMain
    . replaceLambda
    . formFreeVariablesMain
{-# INLINABLE lambdaLifting #-}

replaceLambda :: MainProgram ('AnnotatedWith FreeVariables) l -> MainProgram 'Simple l
replaceLambda
  = _Wrapped . each . _supercombinatorBody %~ replaceLambdaE
{-# INLINABLE replaceLambda #-}

replaceLambdaE :: MainExpression ('AnnotatedWith FreeVariables) l -> MainExpression 'Simple l
replaceLambdaE (EInteger _ n) = EInteger () n
replaceLambdaE (EConstructor _ tag arity) = EConstructor () tag arity
replaceLambdaE (EVariable _ v) = EVariable () v
replaceLambdaE (EPrimitive _ prim) = EPrimitive () prim
replaceLambdaE (EApplication _ e1 e2)
  = EApplication () (replaceLambdaE e1) (replaceLambdaE e2)
replaceLambdaE (ELet _ flag lDefs expr)
  = ELet ()
    flag
    (lDefs & each . _letDefinitionBody %~ replaceLambdaE)
    (replaceLambdaE expr)
replaceLambdaE (EMatch _ expr mCases)
  = EMatch ()
    (replaceLambdaE expr)
    (mCases & each . _matchCaseBody %~ replaceLambdaE)
replaceLambdaE (ELambda fvs args expr)
  = foldl' (EApplication ()) annon (EVariable () <$> fvsList)
  where
    annon
      = ELet ()
        NonRecursive
        [ LetDefinition
          ( "annon"
          , ELambda ()
            (fvsList <> args)
            (replaceLambdaE expr)
          )
        ]
        (EVariable () "annon")
    fvsList = Set.toList fvs

    {-# INLINE annon #-}
    {-# INLINABLE fvsList #-}

liftAnnons :: MainProgram t l -> MainProgram 'Simple 'LLMC
liftAnnons = _Wrapped %~ concatMap liftAnnonsSc
  where
    liftAnnonsSc = uncurry (:) . swap . (_supercombinatorBody %%~ liftAnnonsE)
    {-# INLINE liftAnnonsSc #-}
{-# INLINE liftAnnons #-}

liftAnnonsE :: MainExpression t l -> ([MainSupercombinator 'Simple 'LLMC], MainExpression 'Simple 'LLMC)
liftAnnonsE (EInteger _ n) = (mempty, EInteger () n)
liftAnnonsE (EConstructor _ tag arity) = (mempty, EConstructor () tag arity)
liftAnnonsE (EVariable _ v) = (mempty, EVariable () v)
liftAnnonsE (EPrimitive _ prim) = (mempty, EPrimitive () prim)
liftAnnonsE (EApplication _ e1 e2) = (scs1 <> scs2, EApplication () e1' e2')
  where
    (scs1, e1') = liftAnnonsE e1
    (scs2, e2') = liftAnnonsE e2
liftAnnonsE (ELet _ flag [LetDefinition (v1, ELambda _ args e)] (EVariable _ v2))
  | not (isRecursive flag)
  , v1 == v2
  = (pure (Supercombinator (v2, args, e')) <> scs, EVariable () v2)
  | otherwise
  = error "liftAnnonsE: wrong annonymous pattern is created"
  where
    (scs, e') = liftAnnonsE e
liftAnnonsE (ELet _ flag lDefs expr)
  = (lDefsScs <> exprScs, ELet () flag lDefs' expr')
  where
    (lDefsScs, lDefs') = lDefs & each . _letDefinitionBody %%~ liftAnnonsE
    (exprScs, expr') = liftAnnonsE expr
liftAnnonsE (EMatch _ expr mCases)
  = (exprScs <> mCasesScs, EMatch () expr' mCases')
  where
    (exprScs, expr') = liftAnnonsE expr
    (mCasesScs, mCases') = mCases & each . _matchCaseBody %%~ liftAnnonsE
liftAnnonsE (ELambda _ _ _)
  = error "liftAnnonsE: unexpected ELambda expression"
