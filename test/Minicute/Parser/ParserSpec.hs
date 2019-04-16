module Minicute.Parser.ParserSpec
  ( spec
  ) where

import Test.Hspec
import Test.Hspec.Megaparsec

import Control.Monad
import Minicute.Data.Tuple
import Minicute.Types.Program
import Text.Megaparsec

import qualified Minicute.Parser.Parser as P

spec :: Spec
spec = do
  describe "programL parser" $ do
    forM_ testCases (\(name, content, result) -> programLTest name content result)

programLTest :: String -> String -> MainProgramL -> SpecWith (Arg Expectation)
programLTest name content result = do
  it ("parses " <> name <> " successfully") $ do
    parse P.programL "" content `shouldParse` result

type TestName = String
type TestContent = String
type TestResult = MainProgramL
type TestCase = (TestName, TestContent, TestResult)

testCases :: [TestCase]
testCases
  = fmap tupleUnzip2 (zip simpleTestLabels simpleTestCases)
  where
    simpleTestLabels = fmap (("simple case" ++) . show) [0..]

simpleTestCases :: [(TestContent, TestResult)]
simpleTestCases
  = [ ( "f = 1"
      , ProgramL
        [ ( "f"
          , []
          , ELNum 1
          )
        ]
      )
    , ( "f = 1;"
      , ProgramL
        [ ( "f"
          , []
          , ELNum 1
          )
        ]
      )
    , ( "f=1;"
      , ProgramL
        [ ( "f"
          , []
          , ELNum 1
          )
        ]
      )
    , ( " f= 1;"
      , ProgramL
        [ ( "f"
          , []
          , ELNum 1
          )
        ]
      )
    , ( " f= 1 ;  "
      , ProgramL
        [ ( "f"
          , []
          , ELNum 1
          )
        ]
      )
    , ( "f = 1;\ng = 2"
      , ProgramL
        [ ( "f"
          , []
          , ELNum 1
          )
        , ( "g"
          , []
          , ELNum 2
          )
        ]
      )
    , ( "f = 1  ;  \n g=2 ;"
      , ProgramL
        [ ( "f"
          , []
          , ELNum 1
          )
        , ( "g"
          , []
          , ELNum 2
          )
        ]
      )
    , ( "f = g;\ng = 2"
      , ProgramL
        [ ( "f"
          , []
          , ELVar "g"
          )
        , ( "g"
          , []
          , ELNum 2
          )
        ]
      )
    ]

arithOpTestCases :: [TestCase]
arithOpTestCases
  = [ ( "addition of two nums"
      , "f = 1 + 1"
      , ProgramL
        [ ( "f"
          , []
          , eLAp2 (ELVar "+") (ELNum 1) (ELNum 1)
          )
        ]
      )
    , ( "addition of num and var"
      , "f = 1 * g;\ng = 3"
      , ProgramL
        [ ( "f"
          , []
          , eLAp2 (ELVar "*") (ELNum 1) (ELVar "g")
          )
        , ( "g"
          , []
          , ELNum 3
          )
        ]
      )
    , ( "operator precedence of + and *"
      , "f = 1 * 2 + 3"
      , ProgramL
        [ ( "f"
          , []
          , eLAp2
            (ELVar "*")
            (eLAp2
              (ELVar "*")
              (ELNum 1)
              (ELNum 2))
            (ELNum 3)
          )
        ]
      )
    ]

eLAp2 :: MainExpressionL -> MainExpressionL -> MainExpressionL -> MainExpressionL
eLAp2 = (ELAp .) . ELAp
