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
  describe "immediateApplicationMainMC" $ do
    forM_ testCases (uncurry3 immediateApplicationMainMCTest)

immediateApplicationMainMCTest :: TestName -> TestBeforeContent -> TestAfterContent -> SpecWith (Arg Expectation)
immediateApplicationMainMCTest name beforeContent afterContent = do
  it ("immediate applications are optimized in " <> name) $ do
    immediateApplicationMainMC beforeContent `shouldBe` afterContent

type TestName = String
type TestBeforeContent = MainProgramMC
type TestAfterContent = MainProgramMC
type TestCase = (TestName, TestBeforeContent, TestAfterContent)

testCases :: [TestCase]
testCases
  = [ ( "program with an application of a one-argument lambda"
      , [qqMiniMainMC|
                    f = (\x -> x) 5
        |]
      , [qqMiniMainMC|
                    f = let
                          x = 5
                        in
                          x
        |]
      )

    , ( "program with an application of a two-argument lambda"
      , [qqMiniMainMC|
                    f = (\x y -> x + y) 5
        |]
      , [qqMiniMainMC|
                    f = \y ->
                          let
                            x = 5
                          in
                            x + y
        |]
      )
    ]
