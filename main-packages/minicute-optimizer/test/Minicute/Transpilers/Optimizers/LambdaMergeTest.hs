{- HLINT ignore "Redundant do" -}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
-- |
-- Copyright: (c) 2018-present Junyoung Clare Jang
-- License: BSD 3-Clause
module Minicute.Transpilers.Optimizers.LambdaMergeTest
  ( spec_lambdaMergeMC
  ) where

import Test.Tasty.Hspec

import Control.Monad
import Data.Tuple.Extra
import Minicute.Transpilers.Optimizers.LambdaMerge
import Minicute.Utils.Minicute.TH

spec_lambdaMergeMC :: Spec
spec_lambdaMergeMC
  = forM_ testCases (uncurry3 lambdaMergeMCTest)

lambdaMergeMCTest :: TestName -> TestBeforeContent -> TestAfterContent -> SpecWith (Arg Expectation)
lambdaMergeMCTest name beforeContent afterContent = do
  it ("immediate applications are optimized in " <> name) $ do
    lambdaMergeMC beforeContent `shouldBe` afterContent

type TestName = String
type TestBeforeContent = MainProgram 'Simple 'MC
type TestAfterContent = MainProgram 'Simple 'MC
type TestCase = (TestName, TestBeforeContent, TestAfterContent)

testCases :: [TestCase]
testCases
  = [ ( "program with two one-argument lambda expressions"
      , [qqMiniMainMC|
                    f = \x -> \y -> x + y
        |]
      , [qqMiniMainMC|
                    f = \x y -> x + y
        |]
      )

    , ( "program with three one-argument lambda expressions"
      , [qqMiniMainMC|
                    f = \x -> \y -> \z -> x + y + z
        |]
      , [qqMiniMainMC|
                    f = \x y z -> x + y + z
        |]
      )

    , ( "program with two two-argument lambda expressions"
      , [qqMiniMainMC|
                    f = \a b -> \c d -> a * d - b * c
        |]
      , [qqMiniMainMC|
                    f = \a b c d -> a * d - b * c
        |]
      )
    ]
