{- HLINT ignore "Redundant do" -}
{-# LANGUAGE QuasiQuotes #-}
module Minicute.Transpilers.Optimizers.LambdaMergeSpec
  ( spec
  ) where

import Test.Hspec

import Control.Monad
import Data.Tuple.Extra
import Minicute.Data.Minicute.Program
import Minicute.Transpilers.Optimizers.LambdaMerge
import Minicute.Utils.Minicute.TH

spec :: Spec
spec = do
  describe "lambdaMergeMainMC" $ do
    forM_ testCases (uncurry3 lambdaMergeMainMCTest)

lambdaMergeMainMCTest :: TestName -> TestBeforeContent -> TestAfterContent -> SpecWith (Arg Expectation)
lambdaMergeMainMCTest name beforeContent afterContent = do
  it ("immediate applications are optimized in " <> name) $ do
    lambdaMergeMainMC beforeContent `shouldBe` afterContent

type TestName = String
type TestBeforeContent = MainProgramMC
type TestAfterContent = MainProgramMC
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
