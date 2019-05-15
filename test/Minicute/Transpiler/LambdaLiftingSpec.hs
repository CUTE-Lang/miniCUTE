{- HLINT ignore "Redundant do" -}
module Minicute.Transpiler.LambdaLiftingSpec
  ( spec
  ) where

import Test.Hspec

import Control.Monad
import Data.Tuple.Extra
import Minicute.Transpiler.LambdaLifting
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
    , Program
      []
    , Program
      []
    )
  ]
