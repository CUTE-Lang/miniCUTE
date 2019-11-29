{- HLINT ignore "Redundant do" -}
{-# LANGUAGE QuasiQuotes #-}
-- |
-- Copyright: (c) 2018-present Junyoung Clare Jang
-- License: BSD 3-Clause
module Minicute.Transpilers.Optimizers.ImmediateApplicationSpec
  ( spec_immediateApplicationMainMC
  ) where

import Test.Tasty.Hspec

import Control.Monad
import Data.Tuple.Extra
import Minicute.Data.Minicute.Program
import Minicute.Transpilers.Optimizers.ImmediateApplication
import Minicute.Utils.Minicute.TH

spec_immediateApplicationMainMC :: Spec
spec_immediateApplicationMainMC
  = forM_ testCases (uncurry3 immediateApplicationMainMCTest)

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
