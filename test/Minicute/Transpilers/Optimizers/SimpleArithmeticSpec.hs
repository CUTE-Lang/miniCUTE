{- HLINT ignore "Redundant do" -}
{-# LANGUAGE QuasiQuotes #-}
module Minicute.Transpilers.Optimizers.SimpleArithmeticSpec
  ( spec
  ) where

import Test.Hspec

import Control.Monad
import Data.Tuple.Extra
import Minicute.Transpilers.Optimizers.SimpleArithmetic
import Minicute.Data.Minicute.Program
import Minicute.Utils.TH

spec :: Spec
spec = do
  describe "simpleArithmeticMainL" $ do
    forM_ testCases (uncurry3 simpleArithmeticMainLTest)

simpleArithmeticMainLTest :: TestName -> TestBeforeContent -> TestAfterContent -> SpecWith (Arg Expectation)
simpleArithmeticMainLTest name beforeContent afterContent = do
  it ("immediate applications are optimized in " <> name) $ do
    simpleArithmeticMainL beforeContent `shouldBe` afterContent

type TestName = String
type TestBeforeContent = MainProgramL
type TestAfterContent = MainProgramL
type TestCase = (TestName, TestBeforeContent, TestAfterContent)

testCases :: [TestCase]
testCases
  = [ ( "program with simple arithmetics as top-level bodies"
      , [qqMiniMainL|
                    f = 1 + 1;
                    g = 2 * 3 + 1;
                    h = (3 - 2) * 3 - 2 * 3 + 3
        |]
      , [qqMiniMainL|
                    f = 2;
                    g = 7;
                    h = 0
        |]
      )

    , ( "program with simple arithmetics in let definitions"
      , [qqMiniMainL|
                    f = let
                          x = 1 + 2
                        in
                          x + 3;
                    g = let
                          x = 2 * 3 + 1;
                          y = let
                                z = 3 - 2
                              in
                                id z
                        in
                          test y x (x + x)
        |]
      , [qqMiniMainL|
                    f = let
                          x = 3
                        in
                          x + 3;
                    g = let
                          x = 7;
                          y = let
                                z = 1
                              in
                                id z
                        in
                          test y x (x + x)
        |]
      )
    ]
