{- HLINT ignore "Redundant do" -}
{-# LANGUAGE QuasiQuotes #-}
module Minicute.Transpiler.VariablesRenamingSpec
  ( spec
  ) where

import Test.Hspec
import Test.Minicute.Utils

import Control.Monad
import Control.Lens
import Data.List
import Minicute.Parser.Parser
import Minicute.Transpiler.VariablesRenaming
import Minicute.Types.Minicute.Precedence
import Minicute.Types.Minicute.Program
import Text.Megaparsec

import qualified Data.Set as Set

spec :: Spec
spec = do
  describe "renameVariablesMainL" $ do
    forM_ testCases (uncurry renameVariablesMainLTest)

-- |
-- These tests are too weak.
-- We should add tests to check whether the dependence is preserved or not.
renameVariablesMainLTest :: TestName -> TestContent -> SpecWith (Arg Expectation)
renameVariablesMainLTest name beforeContent = do
  it ("rename variables to avoid identifier conflicts in " <> name) $ do
    renameVariablesMainL beforeContent `shouldSatisfy` haveNoIdentifierConflictMainL

type TestName = String
type TestContent = MainProgramL
type TestCase = (TestName, TestContent)

testCases :: [TestCase]
testCases
  = [ ( "questionable program0"
      , [qqMini|
               main = f 5 4;
               f x y = letrec
                         g = \z -> x + y + z;
                         h = \x y -> x * y * g 5
                       in
                         h 1 y
        |]
      )
    , ( "questionable program1"
      , [qqMini|
               main = f 5 4;
               f x y = let
                         g = \z -> x + y + z;
                         h = \x y -> x * y * g 5
                       in
                         h 1 y;
               g x = x / 2
        |]
      )
    ]

-- |
-- This should check any conflicts of identifiers from anywhere.
haveNoIdentifierConflictMainL :: MainProgramL -> Bool
haveNoIdentifierConflictMainL (ProgramL scs)
  = scIdsNoConflict
  && (and . snd . mapAccumL haveNoIdentifierConflictMainEL scIdSet $ view supercombinatorBody <$> scs)
  where
    scIdsNoConflict = Set.size scIdSet == length scs
    scIdSet = Set.fromList (view supercombinatorBinder <$> scs)

haveNoIdentifierConflictMainEL :: Set.Set Identifier -> MainExpressionL -> (Set.Set Identifier, Bool)
haveNoIdentifierConflictMainEL env (ELInteger _) = (env, True)
haveNoIdentifierConflictMainEL env (ELConstructor _ _) = (env, True)
haveNoIdentifierConflictMainEL env (ELVariable v) = (env, Set.notMember v env)
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

    lDefBodies = view letDefinitionBody <$> lDefs
    lDefIdSet = Set.fromList lDefIds
    lDefIds = view letDefinitionBinder <$> lDefs
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
    mCaseArgs = concat (view matchCaseArguments <$> mCases)
    mCaseBodies = view matchCaseBody <$> mCases
haveNoIdentifierConflictMainEL env (ELLambda args expr)
  = (exprEnv, argsNoConflict && exprNoConflict)
  where
    argsNoConflict
      = Set.disjoint env argSet
        && Set.size argSet == length args
    (exprEnv, exprNoConflict) = haveNoIdentifierConflictMainEL (argSet <> env) expr

    argSet = Set.fromList args
