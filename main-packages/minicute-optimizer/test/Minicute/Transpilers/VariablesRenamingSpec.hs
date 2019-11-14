{- HLINT ignore "Redundant do" -}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
module Minicute.Transpilers.VariablesRenamingSpec
  ( spec
  ) where

import Test.Hspec

import Control.Lens
import Control.Monad
import Data.List
import Data.Tuple.Extra
import Minicute.Data.Minicute.Program
import Minicute.Transpilers.VariablesRenaming
import Minicute.Utils.Minicute.TH

import qualified Data.Set as Set

spec :: Spec
spec = do
  describe "renameVariablesMainMC" $ do
    forM_ testCases (uncurry3 renameVariablesMainMCTest)

renameVariablesMainMCTest :: TestName -> TestBeforeContent -> TestAfterContent -> SpecWith (Arg Expectation)
renameVariablesMainMCTest name beforeContent afterContent = do
  it ("rename variables to avoid identifier conflicts in " <> name) $ do
    renameVariablesMainMC beforeContent `shouldSatisfy` haveNoIdentifierConflictMainMC
  it ("rename variables to expected result from " <> name) $ do
    renameVariablesMainMC beforeContent `shouldBe` afterContent

type TestName = String
type TestBeforeContent = MainProgramMC
type TestAfterContent = MainProgramMC
type TestCase = (TestName, TestBeforeContent, TestAfterContent)

testCases :: [TestCase]
testCases
  = [ ( "program with let"
      , [qqMiniMainMC|
                    main = f 5 4;
                    f x y = let
                              g = \z -> x + y + z;
                              h = \x y -> x * y * g 5
                            in
                              h 1 y;
                    g x = x / 2
        |]
      , [qqMiniMainMC|
                    main = f0 5 4;
                    f0 x2 y3 = let
                                 g4 = \z6 -> x2 + y3 + z6;
                                 h5 = \x7 y8 -> x7 * y8 * g1 5
                               in
                                 h5 1 y3;
                    g1 x9 = x9 / 2
        |]
      )
    , ( "program with letrec"
      , [qqMiniMainMC|
                    main = f 5 4;
                    f x y = letrec
                              g = \z -> x + y + z;
                              h = \x y -> x * y * g 5
                            in
                              h 1 y
        |]
      , [qqMiniMainMC|
                    main = f0 5 4;
                    f0 x1 y2 = letrec
                                 g3 = \z5 -> x1 + y2 + z5;
                                 h4 = \x6 y7 -> x6 * y7 * g3 5
                               in
                                 h4 1 y2
        |]
      )
    , ( "program with match"
      , [qqMiniMainMC|
                    main = f (g 5);
                    f x = match x with
                            <1> -> 0;
                            <2> h t -> h + f t;
                    g x = if (x > 0) ($C{2;2} x (g (x - 1))) Nil
        |]
      , [qqMiniMainMC|
                    main = f0 (g1 5);
                    f0 x2 = match x2 with
                            <1> -> 0;
                            <2> h3 t4 -> h3 + f0 t4;
                    g1 x5 = if (x5 > 0) ($C{2;2} x5 (g1 (x5 - 1))) Nil
        |]
      )
    ]

-- |
-- This should check any conflicts of identifiers from anywhere.
haveNoIdentifierConflictMainMC :: MainProgramMC -> Bool
haveNoIdentifierConflictMainMC (Program scs)
  = scIdsNoConflict
  && (and . snd . mapAccumL haveNoIdentifierConflictMainEMC scIdSet $ view _supercombinatorBody <$> scs)
  where
    scIdsNoConflict = Set.size scIdSet == length scs
    scIdSet = Set.fromList (view _supercombinatorBinder <$> scs)

haveNoIdentifierConflictMainEMC :: Set.Set Identifier -> MainExpressionMC -> (Set.Set Identifier, Bool)
haveNoIdentifierConflictMainEMC env (EInteger _) = (env, True)
haveNoIdentifierConflictMainEMC env (EConstructor _ _) = (env, True)
haveNoIdentifierConflictMainEMC env (EVariable _) = (env, True)
haveNoIdentifierConflictMainEMC env (EApplication e1 e2)
  = (env2, noConflict1 && noConflict2)
  where
    (env1, noConflict1) = haveNoIdentifierConflictMainEMC env e1
    (env2, noConflict2) = haveNoIdentifierConflictMainEMC env1 e2
haveNoIdentifierConflictMainEMC env (ELet _ lDefs expr)
  = (exprEnv, lDefIdsNoConflict && lDefBodiesNoConflict && exprNoConflict)
  where
    lDefIdsNoConflict
      = Set.disjoint env lDefIdSet
        && length lDefIds == Set.size lDefIdSet
    (lDefEnv, lDefBodiesNoConflict)
      = and <$> mapAccumL haveNoIdentifierConflictMainEMC (lDefIdSet <> env) lDefBodies
    (exprEnv, exprNoConflict)
      = haveNoIdentifierConflictMainEMC lDefEnv expr

    lDefBodies = view _letDefinitionBody <$> lDefs
    lDefIdSet = Set.fromList lDefIds
    lDefIds = view _letDefinitionBinder <$> lDefs
haveNoIdentifierConflictMainEMC env (EMatch expr mCases)
  = (mCaseEnv, exprNoConflict && mCaseArgsNoConflict && mCaseBodiesNoConflict)
  where
    (exprEnv, exprNoConflict)
      = haveNoIdentifierConflictMainEMC (mCaseArgIdSet <> env) expr
    mCaseArgsNoConflict
      = Set.disjoint env mCaseArgIdSet
        && Set.size mCaseArgIdSet == length mCaseArgs
    (mCaseEnv, mCaseBodiesNoConflict)
      = and <$> mapAccumL haveNoIdentifierConflictMainEMC exprEnv mCaseBodies

    mCaseArgIdSet = Set.fromList mCaseArgs
    mCaseArgs = concat (view _matchCaseArguments <$> mCases)
    mCaseBodies = view _matchCaseBody <$> mCases
haveNoIdentifierConflictMainEMC env (ELambda args expr)
  = (exprEnv, argsNoConflict && exprNoConflict)
  where
    argsNoConflict
      = Set.disjoint env argSet
        && Set.size argSet == length args
    (exprEnv, exprNoConflict) = haveNoIdentifierConflictMainEMC (argSet <> env) expr

    argSet = Set.fromList args
