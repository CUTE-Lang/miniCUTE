{- HLINT ignore "Redundant do" -}
{-# LANGUAGE QuasiQuotes #-}
module Minicute.Transpilers.VariablesRenamingSpec
  ( spec
  ) where

import Test.Hspec

import Control.Monad
import Control.Lens
import Data.List
import Data.Tuple.Extra
import Minicute.Transpilers.VariablesRenaming
import Minicute.Types.Minicute.Program
import Minicute.Utils.TH

import qualified Data.Set as Set

spec :: Spec
spec = do
  describe "renameVariablesMainL" $ do
    forM_ testCases (uncurry3 renameVariablesMainLTest)

renameVariablesMainLTest :: TestName -> TestBeforeContent -> TestAfterContent -> SpecWith (Arg Expectation)
renameVariablesMainLTest name beforeContent afterContent = do
  it ("rename variables to avoid identifier conflicts in " <> name) $ do
    renameVariablesMainL beforeContent `shouldSatisfy` haveNoIdentifierConflictMainL
  it ("rename variables to expected result from " <> name) $ do
    renameVariablesMainL beforeContent `shouldBe` afterContent

type TestName = String
type TestBeforeContent = MainProgramL
type TestAfterContent = MainProgramL
type TestCase = (TestName, TestBeforeContent, TestAfterContent)

testCases :: [TestCase]
testCases
  = [ ( "program with let"
      , [qqMiniMainL|
                    main = f 5 4;
                    f x y = let
                              g = \z -> x + y + z;
                              h = \x y -> x * y * g 5
                            in
                              h 1 y;
                    g x = x / 2
        |]
      , [qqMiniMainL|
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
      , [qqMiniMainL|
                    main = f 5 4;
                    f x y = letrec
                              g = \z -> x + y + z;
                              h = \x y -> x * y * g 5
                            in
                              h 1 y
        |]
      , [qqMiniMainL|
                    main = f0 5 4;
                    f0 x1 y2 = letrec
                                 g3 = \z5 -> x1 + y2 + z5;
                                 h4 = \x6 y7 -> x6 * y7 * g3 5
                               in
                                 h4 1 y2
        |]
      )
    , ( "program with match"
      , [qqMiniMainL|
                    main = f (g 5);
                    f x = match x with
                            <1> -> 0;
                            <2> h t -> h + f t;
                    g x = if (x > 0) ($C{2;2} x (g (x - 1))) Nil
        |]
      , [qqMiniMainL|
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
haveNoIdentifierConflictMainL :: MainProgramL -> Bool
haveNoIdentifierConflictMainL (ProgramL scs)
  = scIdsNoConflict
  && (and . snd . mapAccumL haveNoIdentifierConflictMainEL scIdSet $ view _supercombinatorBody <$> scs)
  where
    scIdsNoConflict = Set.size scIdSet == length scs
    scIdSet = Set.fromList (view _supercombinatorBinder <$> scs)

haveNoIdentifierConflictMainEL :: Set.Set Identifier -> MainExpressionL -> (Set.Set Identifier, Bool)
haveNoIdentifierConflictMainEL env (ELInteger _) = (env, True)
haveNoIdentifierConflictMainEL env (ELConstructor _ _) = (env, True)
haveNoIdentifierConflictMainEL env (ELVariable _) = (env, True)
haveNoIdentifierConflictMainEL env (ELApplication e1 e2)
  = (env2, noConflict1 && noConflict2)
  where
    (env1, noConflict1) = haveNoIdentifierConflictMainEL env e1
    (env2, noConflict2) = haveNoIdentifierConflictMainEL env1 e2
haveNoIdentifierConflictMainEL env (ELLet _ lDefs expr)
  = (exprEnv, lDefIdsNoConflict && lDefBodiesNoConflict && exprNoConflict)
  where
    lDefIdsNoConflict
      = Set.disjoint env lDefIdSet
        && length lDefIds == Set.size lDefIdSet
    (lDefEnv, lDefBodiesNoConflict)
      = and <$> mapAccumL haveNoIdentifierConflictMainEL (lDefIdSet <> env) lDefBodies
    (exprEnv, exprNoConflict)
      = haveNoIdentifierConflictMainEL lDefEnv expr

    lDefBodies = view _letDefinitionBody <$> lDefs
    lDefIdSet = Set.fromList lDefIds
    lDefIds = view _letDefinitionBinder <$> lDefs
haveNoIdentifierConflictMainEL env (ELMatch expr mCases)
  = (mCaseEnv, exprNoConflict && mCaseArgsNoConflict && mCaseBodiesNoConflict)
  where
    (exprEnv, exprNoConflict)
      = haveNoIdentifierConflictMainEL (mCaseArgIdSet <> env) expr
    mCaseArgsNoConflict
      = Set.disjoint env mCaseArgIdSet
        && Set.size mCaseArgIdSet == length mCaseArgs
    (mCaseEnv, mCaseBodiesNoConflict)
      = and <$> mapAccumL haveNoIdentifierConflictMainEL exprEnv mCaseBodies

    mCaseArgIdSet = Set.fromList mCaseArgs
    mCaseArgs = concat (view _matchCaseArguments <$> mCases)
    mCaseBodies = view _matchCaseBody <$> mCases
haveNoIdentifierConflictMainEL env (ELLambda args expr)
  = (exprEnv, argsNoConflict && exprNoConflict)
  where
    argsNoConflict
      = Set.disjoint env argSet
        && Set.size argSet == length args
    (exprEnv, exprNoConflict) = haveNoIdentifierConflictMainEL (argSet <> env) expr

    argSet = Set.fromList args
