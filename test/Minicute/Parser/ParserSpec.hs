{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Minicute.Parser.ParserSpec
  ( spec
  ) where

import Test.Hspec
import Test.Hspec.Megaparsec

import Control.Monad
import Minicute.Data.Tuple ( tupleUnzip2 )
import Minicute.Types.Minicute.Program
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
  = simpleTestCases
    <> arithmeticOperatorTestCases
    <> constructorTestCases
    <> applicationTestCases
    <> supercombinatorTestCases
  where
    simpleTestCases = fmap tupleUnzip2 (zip simpleLabels simpleTestTemplates)
    simpleLabels = fmap (("simple case" <>) . show) [0..]

simpleTestTemplates :: [(TestContent, TestResult)]
simpleTestTemplates
  = [ ( "f = 1"
      , ProgramL
        [ ( "f"
          , []
          , ELInteger 1
          )
        ]
      )
    , ( "f = 1;"
      , ProgramL
        [ ( "f"
          , []
          , ELInteger 1
          )
        ]
      )
    , ( "f=1;"
      , ProgramL
        [ ( "f"
          , []
          , ELInteger 1
          )
        ]
      )
    , ( " f= 1;"
      , ProgramL
        [ ( "f"
          , []
          , ELInteger 1
          )
        ]
      )
    , ( " f= 1 ;  "
      , ProgramL
        [ ( "f"
          , []
          , ELInteger 1
          )
        ]
      )
    , ( "f = 1;\ng = 2"
      , ProgramL
        [ ( "f"
          , []
          , ELInteger 1
          )
        , ( "g"
          , []
          , ELInteger 2
          )
        ]
      )
    , ( "f = 1  ;  \n g=2 ;"
      , ProgramL
        [ ( "f"
          , []
          , ELInteger 1
          )
        , ( "g"
          , []
          , ELInteger 2
          )
        ]
      )
    , ( "f = g;\ng = 2"
      , ProgramL
        [ ( "f"
          , []
          , ELVariable "g"
          )
        , ( "g"
          , []
          , ELInteger 2
          )
        ]
      )
    ]

arithmeticOperatorTestCases :: [TestCase]
arithmeticOperatorTestCases
  = [ ( "addition of two nums"
      , "f = 1 + 1"
      , ProgramL
        [ ( "f"
          , []
          , ELApplication2 (ELVariable "+") (ELInteger 1) (ELInteger 1)
          )
        ]
      )
    , ( "addition of num and var"
      , "f = 1 * g;\ng = 3"
      , ProgramL
        [ ( "f"
          , []
          , ELApplication2 (ELVariable "*") (ELInteger 1) (ELVariable "g")
          )
        , ( "g"
          , []
          , ELInteger 3
          )
        ]
      )
    , ( "operator precedence of + and *"
      , "f = 1 * 2 + 3"
      , ProgramL
        [ ( "f"
          , []
          , ELApplication2
            (ELVariable "+")
            (ELApplication2
              (ELVariable "*")
              (ELInteger 1)
              (ELInteger 2))
            (ELInteger 3)
          )
        ]
      )
    ]

constructorTestCases :: [TestCase]
constructorTestCases
  = [ ( "basic constructor"
      , "f = Pack{1,0};g = Pack{2,2}"
      , ProgramL
        [ ( "f"
          , []
          , ELConstructor 1 0
          )
        , ( "g"
          , []
          , ELConstructor 2 2
          )
        ]
      )
    , ( "constructor with arguments"
      , "f = Pack{1,1} 5;g = Pack{2,3} f"
      , ProgramL
        [ ( "f"
          , []
          , ELApplication (ELConstructor 1 1) (ELInteger 5)
          )
        , ( "g"
          , []
          , ELApplication (ELConstructor 2 3) (ELVariable "f")
          )
        ]
      )
    ]

applicationTestCases :: [TestCase]
applicationTestCases
  = [ ( "application of an integer"
      , "f = g 5"
      , ProgramL
        [ ( "f"
          , []
          , ELApplication (ELVariable "g") (ELInteger 5)
          )
        ]
      )
    , ( "application of a variable"
      , "f = g f"
      , ProgramL
        [ ( "f"
          , []
          , ELApplication (ELVariable "g") (ELVariable "f")
          )
        ]
      )
    ]

supercombinatorTestCases :: [TestCase]
supercombinatorTestCases
  = [ ( "supercombinator with an argument"
      , "f x = x"
      , ProgramL
        [ ( "f"
          , ["x"]
          , ELVariable "x"
          )
        ]
      )
    , ( "supercombinator with two argument"
      , "f x y = x y"
      , ProgramL
        [ ( "f"
          , ["x", "y"]
          , ELApplication (ELVariable "x") (ELVariable "y")
          )
        ]
      )
    ]
