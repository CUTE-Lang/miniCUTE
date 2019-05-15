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

-- |
-- TODO - Introduce quosiquoter for 'Program' and update
-- these test cases
testCases :: [TestCase]
testCases =
  [ ( "empty program"
    , ProgramL
      []
    , Program
      []
    )

  , ( "program with single lambda as a body of a top-level definition"
    , ProgramL
      [ ( "f"
        , []
        , ELLambda ["x"] (ELVariable "x")
        )
      ]
    , Program
      [ ( "f0"
        , []
        , EVariable "annon1"
        )
      , ( "annon1"
        , ["x2"]
        , EVariable "x2"
        )
      ]
    )

  , ( "program with let expression containing lambdas"
    , ProgramL
      [ ( "f"
        , []
        , ELLet
          NonRecursive
          [ ( "g"
            , ELLambda ["x"] (ELVariable "x")
            )
          , ( "h"
            , ELLambda ["x"] (ELApplication2 (ELVariable "*") (ELVariable "x") (ELVariable "x"))
            )
          ]
          ( ELApplication2
            (ELVariable "+")
            (ELApplication (ELVariable "g") (ELInteger 5))
            (ELApplication (ELVariable "h") (ELInteger 4))
          )
        )
      ]
    , Program
      [ ( "f0"
        , []
        , ELet
          NonRecursive
          [ ( "g1"
            , EVariable "annon3"
            )
          , ( "h2"
            , EVariable "annon5"
            )
          ]
          ( EApplication2
            (EVariable "+")
            (EApplication (EVariable "g1") (EInteger 5))
            (EApplication (EVariable "h2") (EInteger 4))
          )
        )
      , ( "annon3"
        , ["x4"]
        , EVariable "x4"
        )
      , ( "annon5"
        , ["x6"]
        , EApplication2 (EVariable "*") (EVariable "x6") (EVariable "x6")
        )
      ]
    )
  ]
