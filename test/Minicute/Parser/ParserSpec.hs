{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Minicute.Parser.ParserSpec
  ( spec
  ) where

import Test.Hspec
import Test.Hspec.Megaparsec

import Control.Monad
import Minicute.Data.Tuple
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
    <> arithOpTestCases
    <> constructorTestCases
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

arithOpTestCases :: [TestCase]
arithOpTestCases
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
      , "f = Pack{1,0}"
      , ProgramL
        [ ( "f"
          , []
          , ELConstructor 1 0
          )
        ]
      )
    , ( "constructor with arguments"
      , "f = Pack{1,1} 5"
      , ProgramL
        [ ( "f"
          , []
          , ELApplication (ELConstructor 1 1) (ELInteger 5)
          )
        ]
      )
    ]
