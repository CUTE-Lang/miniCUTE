{- HLINT ignore "Redundant do" -}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE QuasiQuotes #-}
module Minicute.Parser.ParserSpec
  ( spec
  ) where

import Test.Hspec
import Test.Hspec.Megaparsec
import Test.Minicute.Utils

import Control.Monad
import Data.Tuple.Extra
import Data.Void
import Minicute.Data.Tuple ( tupleUnzip2 )
import Minicute.Types.Minicute.Program
import Text.Megaparsec

import qualified Minicute.Parser.Parser as P

spec :: Spec
spec = do
  describe "programL parser" $ do
    forM_ testCases (uncurry3 programLTest)

programLTest :: TestName -> TestContent -> TestResult -> SpecWith (Arg Expectation)
programLTest name content (TestSuccess result) = do
  it ("parses " <> name <> " successfully") $ do
    parse P.programL "" content `shouldParse` result
programLTest name content (TestFail parseError) = do
  it ("fails to parse " <> name) $ do
    parse P.programL "" content `shouldFailWith` parseError

type TestName = String
type TestContent = String
data TestResult
  = TestSuccess MainProgramL
  | TestFail (ParseError String Void)
type TestCase = (TestName, TestContent, TestResult)

testCases :: [TestCase]
testCases
  = simpleTestCases
    <> arithmeticOperatorTestCases
    <> constructorTestCases
    <> applicationTestCases
    <> supercombinatorTestCases
    <> letAndLetrecTestCases
    <> matchTestCases
    <> lambdaTestCases
    <> complexTestCases
  where
    simpleTestCases = fmap tupleUnzip2 (zip simpleLabels simpleTestTemplates)
    simpleLabels = fmap (("simple case" <>) . show) [0..]

simpleTestTemplates :: [(TestContent, TestResult)]
simpleTestTemplates
  = [ ( [qqMini||]
      , TestSuccess
        ( ProgramL
          [
          ]
        )
      )
    , ( [qqMini|
               f = 1
        |]
      , TestSuccess
        ( ProgramL
          [ ( "f"
            , []
            , ELInteger 1
            )
          ]
        )
      )
    , ( [qqMini|
               f = 1;
        |]
      , TestSuccess
        ( ProgramL
          [ ( "f"
            , []
            , ELInteger 1
            )
          ]
        )
      )
    , ( [qqMini|
               f=1;
        |]
      , TestSuccess
        ( ProgramL
          [ ( "f"
            , []
            , ELInteger 1
            )
          ]
        )
      )
    , ( [qqMini|
               f= 1;
        |]
      , TestSuccess
        ( ProgramL
          [ ( "f"
            , []
            , ELInteger 1
            )
          ]
        )
      )
    , ( [qqMini|
               f= 1 ;
        |]
      , TestSuccess
        ( ProgramL
          [ ( "f"
            , []
            , ELInteger 1
            )
          ]
        )
      )
    , ( [qqMini|
               f = 1;
          g = 2
           |]
      , TestSuccess
        ( ProgramL
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
      )
    , ( [qqMini|
               f = 1  ;
          g=2 ;
           |]
      , TestSuccess
        ( ProgramL
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
      )
    , ( [qqMini|
               f = g;
          g = 2
           |]
      , TestSuccess
        ( ProgramL
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
      )
    , ( [qqMini|
               matchx = matchx
        |]
      , TestSuccess
        ( ProgramL
          [ ( "matchx"
            , []
            , ELVariable "matchx"
            )
          ]
        )
      )
    , ( [qqMini|
               1f = 2
        |]
      , TestFail
        (err 0 (utok '1' <> elabel "identifier" <> eeof))
      )
    , ( [qqMini|
               f;
        |]
      , TestFail
        (err 1 (utok ';' <> etok '=' <> elabel "alphanumeric character" <> etok '_' <> elabel "identifier"))
      )
    , ( [qqMini|
               f =;
        |]
      , TestFail
        (err 3 (utok ';' <> elabel "expression"))
      )
    , ( [qqMini|
               f! = 5;
        |]
      , TestFail
        (err 1 (utok '!' <> etok '=' <> elabel "alphanumeric character" <> etok '_' <> elabel "identifier"))
      )
    , ( [qqMini|
               f = 5;;
        |]
      , TestFail
        (err 6 (utok ';' <> elabel "identifier" <> eeof))
      )
    ]

arithmeticOperatorTestCases :: [TestCase]
arithmeticOperatorTestCases
  = [ ( "addition of two numbers"
      , [qqMini|
               f = 1 + 1
        |]
      , TestSuccess
        ( ProgramL
          [ ( "f"
            , []
            , ELApplication2 (ELVariable "+") (ELInteger 1) (ELInteger 1)
            )
          ]
        )
      )
    , ( "addition of a number and a variable"
      , [qqMini|
               f = 1 * g;
               g = 3
        |]
      , TestSuccess
        ( ProgramL
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
      )
    , ( "multiple addition of numbers"
      , [qqMini|
               f = 1 + (3 + 4)
        |]
      , TestSuccess
        ( ProgramL
          [ ( "f"
            , []
            , ELApplication2
              (ELVariable "+")
              (ELInteger 1)
              (ELApplication2 (ELVariable "+") (ELInteger 3) (ELInteger 4))
            )
          ]
        )
      )
    , ( "operator association of -"
      , [qqMini|
               f = 3 - 2 - 1
        |]
      , TestSuccess
        ( ProgramL
          [ ( "f"
            , []
            , ELApplication2
              (ELVariable "-")
              (ELApplication2
               (ELVariable "-")
               (ELInteger 3)
               (ELInteger 2))
              (ELInteger 1)
            )
          ]
        )
      )
    , ( "operator precedence of + and *"
      , [qqMini|
               f = 1 * 2 + 3
        |]
      , TestSuccess
        ( ProgramL
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
      )
    , ( "left partial application of arithmetic operator"
      , [qqMini|
               f = 2 +
        |]
      , TestFail
        (err 7 (ueof <> elabel "expression with parentheses" <> elabel "constructor" <> elabel "integer" <> elabel "variable"))
      )
    , ( "right partial application of arithmetic operator"
      , [qqMini|
               f = + 2
        |]
      , TestFail
        (err 4 (utoks "+ 2" <> elabel "expression"))
      )
    ]

constructorTestCases :: [TestCase]
constructorTestCases
  = [ ( "basic constructor"
      , [qqMini|
               f = $C{1;0};
               g = $C{2;2}
        |]
      , TestSuccess
        ( ProgramL
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
      )
    , ( "constructor with arguments"
      , [qqMini|
               f = $C{1;1} 5;
               g = $C{2;3} f
        |]
      , TestSuccess
        ( ProgramL
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
      )
    , ( "constructor without arity"
      , [qqMini|
               f = $C{1};
        |]
      , TestFail
        (err 8 (utok '}' <> etok ';' <> elabel "decimal digit"))
      )
    , ( "constructor without tag"
      , [qqMini|
               f = $C{;1};
        |]
      , TestFail
        (err 7 (utok ';' <> elabel "integer"))
      )
    , ( "wrong tokens for constructor"
      , [qqMini|
               f = $Co{1;1};
        |]
      , TestFail
        (err 6 (utok 'o'))
      )
    , ( "wrong tokens for constructor"
      , [qqMini|
               f = $C{1,1};
        |]
      , TestFail
        (err 8 (utok ',' <> etok ';' <> elabel "decimal digit"))
      )
    ]

applicationTestCases :: [TestCase]
applicationTestCases
  = [ ( "application of an integer"
      , [qqMini|
               f = g 5
        |]
      , TestSuccess
        ( ProgramL
          [ ( "f"
            , []
            , ELApplication (ELVariable "g") (ELInteger 5)
            )
          ]
        )
      )
    , ( "application of a variable"
      , [qqMini|
               f = g f
        |]
      , TestSuccess
        ( ProgramL
          [ ( "f"
            , []
            , ELApplication (ELVariable "g") (ELVariable "f")
            )
          ]
        )
      )
    , ( "application of wrong expression"
      , [qqMini|
               f = g []
        |]
      , TestFail
        (err 6 (utok '[' <> etok ';' <> elabel "binary operator" <> elabel "constructor" <> elabel "integer" <> elabel "variable" <> elabel "expression with parentheses" <> eeof))
      )
    ]

supercombinatorTestCases :: [TestCase]
supercombinatorTestCases
  = [ ( "supercombinator with an argument"
      , [qqMini|
               f x = x
        |]
      , TestSuccess
        ( ProgramL
          [ ( "f"
            , ["x"]
            , ELVariable "x"
            )
          ]
        )
      )
    , ( "supercombinator with two argument"
      , [qqMini|
               f x y = x y
        |]
      , TestSuccess
        ( ProgramL
          [ ( "f"
            , ["x", "y"]
            , ELApplication (ELVariable "x") (ELVariable "y")
            )
          ]
        )
      )
    , ( "supercombinator with a number"
      , [qqMini|
               f 5 = x
        |]
      , TestFail
        (err 2 (utok '5' <> elabel "identifier" <> etok '='))
      )
    , ( "supercombinator with an illegal argument"
      , [qqMini|
               f $x = $x
        |]
      , TestFail
        (err 2 (utok '$' <> elabel "identifier" <> etok '='))
      )
    ]

letAndLetrecTestCases :: [TestCase]
letAndLetrecTestCases
  = [ ( "let with a single definition"
      , [qqMini|
               f = let x = 5 in x
        |]
      , TestSuccess
        ( ProgramL
          [ ( "f"
            , []
            , ELLet
              NonRecursive
              [ ("x", ELInteger 5)
              ]
              (ELVariable "x")
            )
          ]
        )
      )
    , ( "letrec with a single definition"
      , [qqMini|
               f = letrec
                  x = 5
                in x
        |]
      , TestSuccess
        ( ProgramL
          [ ( "f"
            , []
            , ELLet
              Recursive
              [ ("x", ELInteger 5)
              ]
              (ELVariable "x")
            )
          ]
        )
      )
    , ( "let with multiple definitions"
      , [qqMini|
               f = let
                     x = 5;
                     y = 4
                   in x + y
        |]
      , TestSuccess
        ( ProgramL
          [ ( "f"
            , []
            , ELLet
              NonRecursive
              [ ("x", ELInteger 5)
              , ("y", ELInteger 4)
              ]
              (ELApplication2 (ELVariable "+") (ELVariable "x") (ELVariable "y"))
            )
          ]
        )
      )
    , ( "letrec with multiple definitions"
      , [qqMini|
               f = letrec
                     x = 5;
                     y = x + x;
                     z = x * y
                   in z
        |]
      , TestSuccess
        ( ProgramL
          [ ( "f"
            , []
            , ELLet
              Recursive
              [ ("x", ELInteger 5)
              , ("y", ELApplication2 (ELVariable "+") (ELVariable "x") (ELVariable "x"))
              , ("z", ELApplication2 (ELVariable "*") (ELVariable "x") (ELVariable "y"))
              ]
              (ELVariable "z")
            )
          ]
        )
      )
    , ( "let with nested let"
      , [qqMini|
               f = let
                     x = let
                           k = 5;
                         in k
                   in x
        |]
      , TestSuccess
        ( ProgramL
          [ ( "f"
            , []
            , ELLet
              NonRecursive
              [ ("x", ELLet NonRecursive [ ("k", ELInteger 5) ] (ELVariable "k"))
              ]
              (ELVariable "x")
            )
          ]
        )
      )
    , ( "let with nested letrec"
      , [qqMini|
               f = let x = letrec k = 5 in k; in x
        |]
      , TestSuccess
        ( ProgramL
          [ ( "f"
            , []
            , ELLet
              NonRecursive
              [ ("x", ELLet Recursive [ ("k", ELInteger 5) ] (ELVariable "k"))
              ]
              (ELVariable "x")
            )
          ]
        )
      )
    , ( "letrec with nested let"
      , [qqMini|
               f = letrec x = let k = 5; in k in x
        |]
      , TestSuccess
        ( ProgramL
          [ ( "f"
            , []
            , ELLet
              Recursive
              [ ("x", ELLet NonRecursive [ ("k", ELInteger 5) ] (ELVariable "k"))
              ]
              (ELVariable "x")
            )
          ]
        )
      )
    , ( "letrec with nested letrec"
      , [qqMini|
               f = letrec
                     x = letrec
                           k = 5;
                         in k
                   in x
        |]
      , TestSuccess
        ( ProgramL
          [ ( "f"
            , []
            , ELLet
              Recursive
              [ ("x", ELLet Recursive [ ("k", ELInteger 5) ] (ELVariable "k"))
              ]
              (ELVariable "x")
            )
          ]
        )
      )
    , ( "let with zero definitions"
      , [qqMini|
               f = let in 5
        |]
      , TestFail
        (errFancy 8 (fancy (ErrorFail "let expression should include at least one definition")))
      )
    , ( "let without in"
      , [qqMini|
               f = let x = 5
        |]
      , TestFail
        (err 13 (ueof <> etok ';' <> etoks "in" <> elabel "decimal digit" <> elabel "binary operator" <> elabel "constructor" <> elabel "integer" <> elabel "variable" <> elabel "expression with parentheses"))
      )
    ]

matchTestCases :: [TestCase]
matchTestCases
  = [ ( "match with a single match case"
      , [qqMini|
               f = match $C{1;0} with <1> -> 5
        |]
      , TestSuccess
        ( ProgramL
          [ ( "f"
            , []
            , ELMatch
              (ELConstructor 1 0)
              [ (1, [], ELInteger 5)
              ]
            )
          ]
        )
      )
    , ( "match with multiple match cases"
      , [qqMini|
               f = match $C{2;0} with
                     <1> -> 5;
                     <2> -> 3;
                     <4> -> g
        |]
      , TestSuccess
        ( ProgramL
          [ ( "f"
            , []
            , ELMatch
              (ELConstructor 2 0)
              [ (1, [], ELInteger 5)
              , (2, [], ELInteger 3)
              , (4, [], ELVariable "g")
              ]
            )
          ]
        )
      )
    , ( "match with arguments"
      , [qqMini|
               f = match $C{2;2} 5 4 with
                     <1> x y -> x;
                     <2> a b -> b
        |]
      , TestSuccess
        ( ProgramL
          [ ( "f"
            , []
            , ELMatch
              (ELApplication2 (ELConstructor 2 2) (ELInteger 5) (ELInteger 4))
              [ (1, ["x", "y"], ELVariable "x")
              , (2, ["a", "b"], ELVariable "b")
              ]
            )
          ]
        )
      )
    , ( "match without a case"
      , [qqMini|
               f = match $C{2;0} with
        |]
      , TestFail
        (errFancy 22 (fancy (ErrorFail "match expression should include at least one case")))
      )
    ]

lambdaTestCases :: [TestCase]
lambdaTestCases
  = [ ( "lambda with a single argument"
      , [qqMini|
               f = \x -> x
        |]
      , TestSuccess
        ( ProgramL
          [ ( "f"
            , []
            , ELLambda
              ["x"]
              (ELVariable "x")
            )
          ]
        )
      )
    , ( "lambda with multiple arguments"
      , [qqMini|
               f = \x y -> x + y
        |]
      , TestSuccess
        ( ProgramL
          [ ( "f"
            , []
            , ELLambda
              ["x", "y"]
              (ELApplication2 (ELVariable "+") (ELVariable "x") (ELVariable "y"))
            )
          ]
        )
      )
    , ( "lambda with nested lambda"
      , [qqMini|
               f = \x -> \y -> x + y
        |]
      , TestSuccess
        ( ProgramL
          [ ( "f"
            , []
            , ELLambda
              ["x"]
              ( ELLambda
                ["y"]
                (ELApplication2 (ELVariable "+") (ELVariable "x") (ELVariable "y"))
              )
            )
          ]
        )
      )
    , ( "immidiate application of lambda"
      , [qqMini|
               f = (\x -> x) 5
        |]
      , TestSuccess
        ( ProgramL
          [ ( "f"
            , []
            , ELApplication
              ( ELLambda
                ["x"]
                (ELVariable "x")
              )
              (ELInteger 5)
            )
          ]
        )
      )
    , ( "lambda without body"
      , [qqMini|
               f = \x ->
        |]
      , TestFail
        (err 9 (ueof <> elabel "expression"))
      )
    , ( "lambda without arguments"
      , [qqMini|
               f = \ -> 5
        |]
      , TestFail
        (err 6 (utok '-' <> elabel "identifier"))
      )
    ]

complexTestCases :: [TestCase]
complexTestCases
  = [ ( "indirect right application of let expression"
      , [qqMini|
               f = 5 + (let k = 5 in k)
        |]
      , TestSuccess
        ( ProgramL
          [ ( "f"
            , []
            , ELApplication2
              (ELVariable "+")
              (ELInteger 5)
              (ELLet NonRecursive [("k", ELInteger 5)] (ELVariable "k"))
            )
          ]
        )
      )
    , ( "indirect right application of match expression"
      , [qqMini|
               f = 5 + (match $C{1;0} with <1> -> 5)
        |]
      , TestSuccess
        ( ProgramL
          [ ( "f"
            , []
            , ELApplication2
              (ELVariable "+")
              (ELInteger 5)
              (ELMatch (ELConstructor 1 0) [(1, [], ELInteger 5)])
            )
          ]
        )
      )
    , ( "direct right application of let expression"
      , [qqMini|
               f = 5 + let k = 5 in k
        |]
      , TestFail
        (errFancy 8 (fancy (ErrorFail "keyword \"let\" cannot be an identifier")))
      )
    , ( "direct right application of match expression"
      , [qqMini|
               f = 5 + match $C{1;0} with <1> -> 5
        |]
      , TestFail
        (errFancy 8 (fancy (ErrorFail "keyword \"match\" cannot be an identifier")))
      )
    ]
