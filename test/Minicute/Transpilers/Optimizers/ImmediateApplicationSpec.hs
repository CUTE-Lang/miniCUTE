{- HLINT ignore "Redundant do" -}
{-# LANGUAGE QuasiQuotes #-}
module Minicute.Transpilers.Optimizers.ImmediateApplicationSpec
  ( spec
  ) where

import Test.Hspec

import Control.Monad
import Data.Tuple.Extra
import Minicute.Transpilers.Optimizers.ImmediateApplication
import Minicute.Data.Minicute.Program
import Minicute.Utils.TH

spec :: Spec
spec = do
  describe "immediateApplicationMainL" $ do
    forM_ testCases (uncurry3 immediateApplicationMainLTest)

immediateApplicationMainLTest :: TestName -> TestBeforeContent -> TestAfterContent -> SpecWith (Arg Expectation)
immediateApplicationMainLTest name beforeContent afterContent = do
  it ("immediate applications are optimized in " <> name) $ do
    immediateApplicationMainL beforeContent `shouldBe` afterContent

type TestName = String
type TestBeforeContent = MainProgramL
type TestAfterContent = MainProgramL
type TestCase = (TestName, TestBeforeContent, TestAfterContent)

testCases :: [TestCase]
testCases
  = [ ( "program with an application of a one-argument lambda"
      , [qqMiniMainL|
                    f = (\x -> x) 5
        |]
      , [qqMiniMainL|
                    f = let
                          x = 5
                        in
                          x
        |]
      )

    , ( "program with an application of a two-argument lambda"
      , [qqMiniMainL|
                    f = (\x y -> x + y) 5
        |]
      , [qqMiniMainL|
                    f = \y ->
                          let
                            x = 5
                          in
                            x + y
        |]
      )
    ]
