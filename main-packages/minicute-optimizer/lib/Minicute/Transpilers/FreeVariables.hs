{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
-- |
-- Copyright: (c) 2018-present Junyoung Clare Jang
-- License: BSD 3-Clause
--
-- Transpilers to extract free variable information of expressions
module Minicute.Transpilers.FreeVariables
  ( module Minicute.Data.Minicute.Program

  , FreeVariables

  , formFreeVariablesMain
  , formFreeVariables
  ) where

import Control.Lens.Each
import Control.Lens.Getter ( Getting, to )
import Control.Lens.Operators
import Control.Lens.Type
import Control.Lens.Wrapped ( _Wrapped )
import Control.Monad.Reader
import Minicute.Data.Minicute.Program

import qualified Data.Set as Set
import qualified Data.Set.Lens as Set

-- |
-- A set of identifiers that are free in the annotated expression
type FreeVariables = Set.Set Identifier

-- |
-- A transpiler to create free variable information for 'MainProgram'
formFreeVariablesMain :: MainProgram t l -> MainProgram ('AnnotatedWith FreeVariables) l
formFreeVariablesMain = formFreeVariables id
{-# INLINABLE formFreeVariablesMain #-}

-- |
-- A transpiler to create free variable information for 'Program'
formFreeVariables :: Getter a Identifier -> Program t l a -> Program ('AnnotatedWith FreeVariables) l a
formFreeVariables _a
  = _Wrapped . each %~ formFreeVariablesSc
    where
      formFreeVariablesSc sc
        = sc & _supercombinatorBody %~ flip runReader scArgsSet . formFVsE _a
        where
          scArgsSet = sc ^. _supercombinatorArguments . setFrom (each . _a)

      {-# INLINABLE formFreeVariablesSc #-}
{-# INLINABLE formFreeVariables #-}

-- |
-- Set of identifiers those are candidates of free variables
type FVELEnvironment = Set.Set Identifier

type FVFormer e e' = e -> Reader FVELEnvironment e'

formFVsE :: Getter a Identifier -> FVFormer (Expression t l a) (Expression ('AnnotatedWith FreeVariables) l a)
formFVsE _ (EInteger _ n) = pure (EInteger Set.empty n)
formFVsE _ (EConstructor _ tag arity) = pure (EConstructor Set.empty tag arity)
formFVsE _ (EVariable _ v) = do
  fvs <- asks getFvs
  pure (EVariable fvs v)
  where
    getFvs env
      | Set.member v env = Set.singleton v
      | otherwise = Set.empty

    {-# INLINABLE getFvs #-}
formFVsE _ (EPrimitive _ prim) = pure (EPrimitive Set.empty prim)
formFVsE _a (EApplication _ expr1 expr2) = do
  expr1' <- formFVsE _a expr1
  expr2' <- formFVsE _a expr2
  pure (EApplication (foldMap (^. _annotation) [expr1', expr2']) expr1' expr2')
formFVsE _a (ELet _ flag lDefs expr) = do
  (exprEnv, lDefsEnv) <- asks getEnvs
  expr' <- local (const exprEnv) $ formFVsE _a expr
  lDefs' <- local (const lDefsEnv) $ formLDefsBodies lDefs
  pure (ELet (getFvs lDefs' expr') flag lDefs' expr')
  where
    getEnvs env
      = let
          exprEnv = lDefsBinderIdSet <> env
          lDefsEnv
            | isRecursive flag = exprEnv
            | otherwise = env
        in
          (exprEnv, lDefsEnv)
    lDefsBinderIdSet = lDefs ^. setFrom (each . _letDefinitionBinder . _a)

    formLDefsBodies = each . _letDefinitionBody %%~ formFVsE _a

    getFvs lDefs' expr' = fvsLDefs' <> fvsExpr'
      where
        fvsLDefs'
          | isRecursive flag = fvsLDefsBodies' Set.\\ lDefsBinderIdSet
          | otherwise = fvsLDefsBodies'
        fvsLDefsBodies' = lDefs' ^. each . _letDefinitionBody . _annotation
        fvsExpr'
          = (expr' ^. _annotation) Set.\\ lDefsBinderIdSet

        {-# INLINABLE fvsLDefs' #-}
        {-# INLINABLE fvsLDefsBodies' #-}
        {-# INLINABLE fvsExpr' #-}

    {-# INLINABLE getEnvs #-}
    {-# INLINABLE formLDefsBodies #-}
    {-# INLINABLE getFvs #-}
formFVsE _a (EMatch _ expr mCases) = do
  expr' <- formFVsE _a expr
  mCases' <- zipWithM formMCase mCasesArgumentSets mCases
  pure (EMatch (getFvs expr' mCases') expr' mCases')
  where
    mCasesArgumentSets = mCases ^.. each . _matchCaseArguments . setFrom (each . _a)

    formMCase mCaseArgSet
      = _matchCaseBody %%~ local (mCaseArgSet <>) . formFVsE _a

    getFvs expr' mCases' = fvsMCases' <> expr' ^. _annotation
      where
        fvssMCasesBodies' = mCases' ^.. each . _matchCaseBody . _annotation
        fvsMCases' = mconcat (zipWith (Set.\\) fvssMCasesBodies' mCasesArgumentSets)

        {-# INLINABLE fvssMCasesBodies' #-}
        {-# INLINABLE fvsMCases' #-}

    {-# INLINABLE formMCase #-}
    {-# INLINABLE getFvs #-}
formFVsE _a (ELambda _ args expr) = do
  expr' <- local (argIdSet <>) $ formFVsE _a expr
  pure (ELambda (getFvs expr') args expr')
  where
    argIdSet = args ^. setFrom (each . _a)

    getFvs expr' = (expr' ^. _annotation) Set.\\ argIdSet

    {-# INLINABLE getFvs #-}

-- |
-- __TODO: move this definition into a separate utility module.__
setFrom :: Getting (Set.Set a) s a -> Getter s (Set.Set a)
setFrom = to . Set.setOf
{-# INLINABLE setFrom #-}
