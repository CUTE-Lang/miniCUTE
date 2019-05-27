{- HLINT ignore "Redundant do" -}
{-# LANGUAGE QuasiQuotes #-}
module Minicute.Transpilers.LambdaLiftingSpec
  ( spec
  ) where

import Test.Hspec
import Test.Minicute.Utils

import Control.Monad
import Data.Tuple.Extra
import Minicute.Transpilers.LambdaLifting
import Minicute.Types.Minicute.Program

spec :: Spec
spec = do
  describe "lambdaLifting" $ do
    forM_ testCases (uncurry3 lambdaLiftingTest)

lambdaLiftingTest :: TestName -> TestBeforeContent -> TestAfterContent -> SpecWith (Arg Expectation)
lambdaLiftingTest name beforeContent afterContent = do
  it ("lift lambda expression from " <> name) $ do
    lambdaLifting beforeContent `shouldBe` afterContent

type TestName = String
type TestBeforeContent = MainProgramL
type TestAfterContent = MainProgram
type TestCase = (TestName, TestBeforeContent, TestAfterContent)

testCases :: [TestCase]
testCases =
  [ ( "empty program"
    , [qqMiniMainL||]
    , [qqMiniMain||]
    )

  , ( "program with single lambda as a body of a top-level definition"
    , [qqMiniMainL|
                  f = \x -> x
      |]
    , [qqMiniMain|
                 f0 = annon1;
                 annon1 x2 = x2
      |]
    )

  , ( "program with let expression containing lambdas"
    , [qqMiniMainL|
                  f = let
                        g = \x -> x;
                        h = \x -> x * x
                      in
                        g 5 + h 4
      |]
    , [qqMiniMain|
                 f0 = let
                        g1 = annon3;
                        h2 = annon5
                      in
                        g1 5 + h2 4;
                 annon3 x4 = x4;
                 annon5 x6 = x6 * x6
      |]
    )
  ]
