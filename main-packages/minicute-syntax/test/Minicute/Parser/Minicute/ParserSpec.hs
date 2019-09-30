{- HLINT ignore "Redundant do" -}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Minicute.Parser.Minicute.ParserSpec
  ( spec
  ) where

import Test.Hspec
import Test.Hspec.Megaparsec

import Control.Monad
import Data.Tuple.Minicute ( tupleUnzip2 )
import Data.Void
import Minicute.Data.Minicute.Program
import Minicute.Utils.TH.Minicute
import Text.Megaparsec

import qualified Minicute.Parser.Minicute.Parser as P

spec :: Spec
spec = do
  describe "mainProgramMC parser" $ do
    forM_ mainProgramMCTestCases mainProgramMCTest

mainProgramMCTest :: MainProgramMCTestCase -> SpecWith (Arg Expectation)
mainProgramMCTest (name, content, Right result) = do
  it ("parses " <> name <> " successfully") $ do
    parse P.mainProgramMC "" content `shouldParse` result
mainProgramMCTest (name, content, Left parseError) = do
  it ("fails to parse " <> name) $ do
    parse P.mainProgramMC "" content `shouldFailWith` parseError

type TestName = String
type TestContent = String
type MainProgramMCTestResult = Either (ParseError String Void) MainProgramMC
type MainProgramMCTestCase = (TestName, TestContent, MainProgramMCTestResult)

mainProgramMCTestCases :: [MainProgramMCTestCase]
mainProgramMCTestCases
  = simpleMainProgramMCTestCases
    <> arithmeticOperatorMainProgramMCTestCases
    <> constructorMainProgramMCTestCases
    <> applicationMainProgramMCTestCases
    <> supercombinatorMainProgramMCTestCases
    <> letAndLetrecMainProgramMCTestCases
    <> matchMainProgramMCTestCases
    <> lambdaMainProgramMCTestCases
    <> complexMainProgramMCTestCases
  where
    simpleMainProgramMCTestCases
      = tupleUnzip2 <$> zip simpleLabels simpleMainProgramMCTestTemplates
    simpleLabels = ("simple case" <>) . show <$> [0..]

simpleMainProgramMCTestTemplates :: [(TestContent, MainProgramMCTestResult)]
simpleMainProgramMCTestTemplates
  = [ ( [qqRawCode||]
      , Right
        ( Program
          [
          ]
        )
      )
    , ( [qqRawCode|
                  f = 1
        |]
      , Right
        ( Program
          [ Supercombinator
            ( "f"
            , []
            , EInteger 1
            )
          ]
        )
      )
    , ( [qqRawCode|
                  f = 1;
        |]
      , Right
        ( Program
          [ Supercombinator
            ( "f"
            , []
            , EInteger 1
            )
          ]
        )
      )
    , ( [qqRawCode|
                  f=1;
        |]
      , Right
        ( Program
          [ Supercombinator
            ( "f"
            , []
            , EInteger 1
            )
          ]
        )
      )
    , ( [qqRawCode|
                  f= 1;
        |]
      , Right
        ( Program
          [ Supercombinator
            ( "f"
            , []
            , EInteger 1
            )
          ]
        )
      )
    , ( [qqRawCode|
                  f= 1 ;
        |]
      , Right
        ( Program
          [ Supercombinator
            ( "f"
            , []
            , EInteger 1
            )
          ]
        )
      )
    , ( [qqRawCode|
                  f = 1;
                  g = 2
        |]
      , Right
        ( Program
          [ Supercombinator
            ( "f"
            , []
            , EInteger 1
            )
          , Supercombinator
            ( "g"
            , []
            , EInteger 2
            )
          ]
        )
      )
    , ( [qqRawCode|
                  f = 1  ;
                  g=2 ;
        |]
      , Right
        ( Program
          [ Supercombinator
            ( "f"
            , []
            , EInteger 1
            )
          , Supercombinator
            ( "g"
            , []
            , EInteger 2
            )
          ]
        )
      )
    , ( [qqRawCode|
                  f = g;
                  g = 2
        |]
      , Right
        ( Program
          [ Supercombinator
            ( "f"
            , []
            , EVariable "g"
            )
          , Supercombinator
            ( "g"
            , []
            , EInteger 2
            )
          ]
        )
      )
    , ( [qqRawCode|
                  matchx = matchx
        |]
      , Right
        ( Program
          [ Supercombinator
            ( "matchx"
            , []
            , EVariable "matchx"
            )
          ]
        )
      )
    , ( [qqRawCode|
                  1f = 2
        |]
      , Left
        (err 0 (utok '1' <> elabel "identifier" <> eeof))
      )
    , ( [qqRawCode|
                  f;
        |]
      , Left
        (err 1 (utok ';' <> etok '=' <> elabel "alphanumeric character" <> etok '_' <> elabel "identifier"))
      )
    , ( [qqRawCode|
                  f =;
        |]
      , Left
        (err 3 (utok ';' <> elabel "expression"))
      )
    , ( [qqRawCode|
                  f! = 5;
        |]
      , Left
        (err 1 (utok '!' <> etok '=' <> elabel "alphanumeric character" <> etok '_' <> elabel "identifier"))
      )
    , ( [qqRawCode|
                  f = 5;;
        |]
      , Left
        (err 6 (utok ';' <> elabel "identifier" <> eeof))
      )
    ]

arithmeticOperatorMainProgramMCTestCases :: [MainProgramMCTestCase]
arithmeticOperatorMainProgramMCTestCases
  = [ ( "addition of two numbers"
      , [qqRawCode|
                  f = 1 + 1
        |]
      , Right
        ( Program
          [ Supercombinator
            ( "f"
            , []
            , EApplication2 (EVariable "+") (EInteger 1) (EInteger 1)
            )
          ]
        )
      )
    , ( "addition of a number and a variable"
      , [qqRawCode|
                  f = 1 * g;
                  g = 3
        |]
      , Right
        ( Program
          [ Supercombinator
            ( "f"
            , []
            , EApplication2 (EVariable "*") (EInteger 1) (EVariable "g")
            )
          , Supercombinator
            ( "g"
            , []
            , EInteger 3
            )
          ]
        )
      )
    , ( "multiple addition of numbers"
      , [qqRawCode|
                  f = 1 + (3 + 4)
        |]
      , Right
        ( Program
          [ Supercombinator
            ( "f"
            , []
            , EApplication2
              (EVariable "+")
              (EInteger 1)
              (EApplication2 (EVariable "+") (EInteger 3) (EInteger 4))
            )
          ]
        )
      )
    , ( "operator association of -"
      , [qqRawCode|
                  f = 3 - 2 - 1
        |]
      , Right
        ( Program
          [ Supercombinator
            ( "f"
            , []
            , EApplication2
              (EVariable "-")
              (EApplication2
               (EVariable "-")
               (EInteger 3)
               (EInteger 2))
              (EInteger 1)
            )
          ]
        )
      )
    , ( "operator precedence of + and *"
      , [qqRawCode|
                  f = 1 * 2 + 3
        |]
      , Right
        ( Program
          [ Supercombinator
            ( "f"
            , []
            , EApplication2
              (EVariable "+")
              (EApplication2
               (EVariable "*")
               (EInteger 1)
               (EInteger 2))
              (EInteger 3)
            )
          ]
        )
      )
    , ( "left partial application of arithmetic operator"
      , [qqRawCode|
                  f = 2 +
        |]
      , Left
        (err 7 (ueof <> elabel "expression with parentheses" <> elabel "constructor" <> elabel "integer" <> elabel "variable"))
      )
    , ( "right partial application of arithmetic operator"
      , [qqRawCode|
                  f = + 2
        |]
      , Left
        (err 4 (utoks "+ 2" <> elabel "expression"))
      )
    ]

constructorMainProgramMCTestCases :: [MainProgramMCTestCase]
constructorMainProgramMCTestCases
  = [ ( "basic constructor"
      , [qqRawCode|
                  f = $C{1;0};
                  g = $C{2;2}
        |]
      , Right
        ( Program
          [ Supercombinator
            ( "f"
            , []
            , EConstructor 1 0
            )
          , Supercombinator
            ( "g"
            , []
            , EConstructor 2 2
            )
          ]
        )
      )
    , ( "constructor with arguments"
      , [qqRawCode|
                  f = $C{1;1} 5;
                  g = $C{2;3} f
        |]
      , Right
        ( Program
          [ Supercombinator
            ( "f"
            , []
            , EApplication (EConstructor 1 1) (EInteger 5)
            )
          , Supercombinator
            ( "g"
            , []
            , EApplication (EConstructor 2 3) (EVariable "f")
            )
          ]
        )
      )
    , ( "constructor without arity"
      , [qqRawCode|
                  f = $C{1};
        |]
      , Left
        (err 8 (utok '}' <> etok ';' <> elabel "decimal digit"))
      )
    , ( "constructor without tag"
      , [qqRawCode|
                  f = $C{;1};
        |]
      , Left
        (err 7 (utok ';' <> elabel "integer"))
      )
    , ( "wrong tokens for constructor"
      , [qqRawCode|
                  f = $Co{1;1};
        |]
      , Left
        (err 6 (utok 'o'))
      )
    , ( "wrong tokens for constructor"
      , [qqRawCode|
                  f = $C{1,1};
        |]
      , Left
        (err 8 (utok ',' <> etok ';' <> elabel "decimal digit"))
      )
    ]

applicationMainProgramMCTestCases :: [MainProgramMCTestCase]
applicationMainProgramMCTestCases
  = [ ( "application of an integer"
      , [qqRawCode|
                  f = g 5
        |]
      , Right
        ( Program
          [ Supercombinator
            ( "f"
            , []
            , EApplication (EVariable "g") (EInteger 5)
            )
          ]
        )
      )
    , ( "application of a variable"
      , [qqRawCode|
                  f = g f
        |]
      , Right
        ( Program
          [ Supercombinator
            ( "f"
            , []
            , EApplication (EVariable "g") (EVariable "f")
            )
          ]
        )
      )
    , ( "application of wrong expression"
      , [qqRawCode|
                  f = g []
        |]
      , Left
        (err 6 (utok '[' <> etok ';' <> elabel "binary operator" <> elabel "constructor" <> elabel "integer" <> elabel "variable" <> elabel "expression with parentheses" <> eeof))
      )
    ]

supercombinatorMainProgramMCTestCases :: [MainProgramMCTestCase]
supercombinatorMainProgramMCTestCases
  = [ ( "supercombinator with an argument"
      , [qqRawCode|
                  f x = x
        |]
      , Right
        ( Program
          [ Supercombinator
            ( "f"
            , ["x"]
            , EVariable "x"
            )
          ]
        )
      )
    , ( "supercombinator with two argument"
      , [qqRawCode|
                  f x y = x y
        |]
      , Right
        ( Program
          [ Supercombinator
            ( "f"
            , ["x", "y"]
            , EApplication (EVariable "x") (EVariable "y")
            )
          ]
        )
      )
    , ( "supercombinator with a number"
      , [qqRawCode|
                  f 5 = x
        |]
      , Left
        (err 2 (utok '5' <> elabel "identifier" <> etok '='))
      )
    , ( "supercombinator with an illegal argument"
      , [qqRawCode|
                  f $x = $x
        |]
      , Left
        (err 2 (utok '$' <> elabel "identifier" <> etok '='))
      )
    ]

letAndLetrecMainProgramMCTestCases :: [MainProgramMCTestCase]
letAndLetrecMainProgramMCTestCases
  = [ ( "let with a single definition"
      , [qqRawCode|
                  f = let x = 5 in x
        |]
      , Right
        ( Program
          [ Supercombinator
            ( "f"
            , []
            , ELet
              NonRecursive
              [ LetDefinition ("x", EInteger 5)
              ]
              (EVariable "x")
            )
          ]
        )
      )
    , ( "letrec with a single definition"
      , [qqRawCode|
                  f = letrec
                        x = 5
                      in x
        |]
      , Right
        ( Program
          [ Supercombinator
            ( "f"
            , []
            , ELet
              Recursive
              [ LetDefinition ("x", EInteger 5)
              ]
              (EVariable "x")
            )
          ]
        )
      )
    , ( "let with multiple definitions"
      , [qqRawCode|
                  f = let
                        x = 5;
                        y = 4
                      in x + y
        |]
      , Right
        ( Program
          [ Supercombinator
            ( "f"
            , []
            , ELet
              NonRecursive
              [ LetDefinition ("x", EInteger 5)
              , LetDefinition ("y", EInteger 4)
              ]
              (EApplication2 (EVariable "+") (EVariable "x") (EVariable "y"))
            )
          ]
        )
      )
    , ( "letrec with multiple definitions"
      , [qqRawCode|
                  f = letrec
                        x = 5;
                        y = x + x;
                        z = x * y
                      in z
        |]
      , Right
        ( Program
          [ Supercombinator
            ( "f"
            , []
            , ELet
              Recursive
              [ LetDefinition ("x", EInteger 5)
              , LetDefinition ("y", EApplication2 (EVariable "+") (EVariable "x") (EVariable "x"))
              , LetDefinition ("z", EApplication2 (EVariable "*") (EVariable "x") (EVariable "y"))
              ]
              (EVariable "z")
            )
          ]
        )
      )
    , ( "let with nested let"
      , [qqRawCode|
                  f = let
                        x = let
                              k = 5;
                            in k
                      in x
        |]
      , Right
        ( Program
          [ Supercombinator
            ( "f"
            , []
            , ELet
              NonRecursive
              [ LetDefinition ("x", ELet NonRecursive [ LetDefinition ("k", EInteger 5) ] (EVariable "k"))
              ]
              (EVariable "x")
            )
          ]
        )
      )
    , ( "let with nested letrec"
      , [qqRawCode|
                  f = let x = letrec k = 5 in k; in x
        |]
      , Right
        ( Program
          [ Supercombinator
            ( "f"
            , []
            , ELet
              NonRecursive
              [ LetDefinition ("x", ELet Recursive [ LetDefinition ("k", EInteger 5) ] (EVariable "k"))
              ]
              (EVariable "x")
            )
          ]
        )
      )
    , ( "letrec with nested let"
      , [qqRawCode|
                  f = letrec x = let k = 5; in k in x
        |]
      , Right
        ( Program
          [ Supercombinator
            ( "f"
            , []
            , ELet
              Recursive
              [ LetDefinition ("x", ELet NonRecursive [ LetDefinition ("k", EInteger 5) ] (EVariable "k"))
              ]
              (EVariable "x")
            )
          ]
        )
      )
    , ( "letrec with nested letrec"
      , [qqRawCode|
                  f = letrec
                        x = letrec
                              k = 5;
                            in k
                      in x
        |]
      , Right
        ( Program
          [ Supercombinator
            ( "f"
            , []
            , ELet
              Recursive
              [ LetDefinition ("x", ELet Recursive [ LetDefinition ("k", EInteger 5) ] (EVariable "k"))
              ]
              (EVariable "x")
            )
          ]
        )
      )
    , ( "let with zero definitions"
      , [qqRawCode|
                  f = let in 5
        |]
      , Left
        (errFancy 8 (fancy (ErrorFail "let expression should include at least one definition")))
      )
    , ( "let without in"
      , [qqRawCode|
                  f = let x = 5
        |]
      , Left
        (err 13 (ueof <> etok ';' <> etoks "in" <> elabel "decimal digit" <> elabel "binary operator" <> elabel "constructor" <> elabel "integer" <> elabel "variable" <> elabel "expression with parentheses"))
      )
    ]

matchMainProgramMCTestCases :: [MainProgramMCTestCase]
matchMainProgramMCTestCases
  = [ ( "match with a single match case"
      , [qqRawCode|
                  f = match $C{1;0} with <1> -> 5
        |]
      , Right
        ( Program
          [ Supercombinator
            ( "f"
            , []
            , EMatch
              (EConstructor 1 0)
              [ MatchCase (1, [], EInteger 5)
              ]
            )
          ]
        )
      )
    , ( "match with multiple match cases"
      , [qqRawCode|
                  f = match $C{2;0} with
                        <1> -> 5;
                        <2> -> 3;
                        <4> -> g
        |]
      , Right
        ( Program
          [ Supercombinator
            ( "f"
            , []
            , EMatch
              (EConstructor 2 0)
              [ MatchCase (1, [], EInteger 5)
              , MatchCase (2, [], EInteger 3)
              , MatchCase (4, [], EVariable "g")
              ]
            )
          ]
        )
      )
    , ( "match with arguments"
      , [qqRawCode|
                  f = match $C{2;2} 5 4 with
                        <1> x y -> x;
                        <2> a b -> b
        |]
      , Right
        ( Program
          [ Supercombinator
            ( "f"
            , []
            , EMatch
              (EApplication2 (EConstructor 2 2) (EInteger 5) (EInteger 4))
              [ MatchCase (1, ["x", "y"], EVariable "x")
              , MatchCase (2, ["a", "b"], EVariable "b")
              ]
            )
          ]
        )
      )
    , ( "match followed by other top-level definition"
      , [qqRawCode|
                  f = match $C{2;2} 5 4 with
                        <1> x y -> x;
                        <2> a b -> b;
                  g = 1
        |]
      , Right
        ( Program
          [ Supercombinator
            ( "f"
            , []
            , EMatch
              (EApplication2 (EConstructor 2 2) (EInteger 5) (EInteger 4))
              [ MatchCase (1, ["x", "y"], EVariable "x")
              , MatchCase (2, ["a", "b"], EVariable "b")
              ]
            )
          , Supercombinator
            ( "g"
            , []
            , EInteger 1
            )
          ]
        )
      )
    , ( "match without a case"
      , [qqRawCode|
                  f = match $C{2;0} with
        |]
      , Left
        (errFancy 22 (fancy (ErrorFail "match expression should include at least one case")))
      )
    ]

lambdaMainProgramMCTestCases :: [MainProgramMCTestCase]
lambdaMainProgramMCTestCases
  = [ ( "lambda with a single argument"
      , [qqRawCode|
                  f = \x -> x
        |]
      , Right
        ( Program
          [ Supercombinator
            ( "f"
            , []
            , ELambda
              ["x"]
              (EVariable "x")
            )
          ]
        )
      )
    , ( "lambda with multiple arguments"
      , [qqRawCode|
                  f = \x y -> x + y
        |]
      , Right
        ( Program
          [ Supercombinator
            ( "f"
            , []
            , ELambda
              ["x", "y"]
              (EApplication2 (EVariable "+") (EVariable "x") (EVariable "y"))
            )
          ]
        )
      )
    , ( "lambda with nested lambda"
      , [qqRawCode|
                  f = \x -> \y -> x + y
        |]
      , Right
        ( Program
          [ Supercombinator
            ( "f"
            , []
            , ELambda
              ["x"]
              ( ELambda
                ["y"]
                (EApplication2 (EVariable "+") (EVariable "x") (EVariable "y"))
              )
            )
          ]
        )
      )
    , ( "immidiate application of lambda"
      , [qqRawCode|
                  f = (\x -> x) 5
        |]
      , Right
        ( Program
          [ Supercombinator
            ( "f"
            , []
            , EApplication
              ( ELambda
                ["x"]
                (EVariable "x")
              )
              (EInteger 5)
            )
          ]
        )
      )
    , ( "lambda without body"
      , [qqRawCode|
                  f = \x ->
        |]
      , Left
        (err 9 (ueof <> elabel "expression"))
      )
    , ( "lambda without arguments"
      , [qqRawCode|
                  f = \ -> 5
        |]
      , Left
        (err 6 (utok '-' <> elabel "identifier"))
      )
    ]

complexMainProgramMCTestCases :: [MainProgramMCTestCase]
complexMainProgramMCTestCases
  = [ ( "indirect right application of let expression"
      , [qqRawCode|
                  f = 5 + (let k = 5 in k)
        |]
      , Right
        ( Program
          [ Supercombinator
            ( "f"
            , []
            , EApplication2
              (EVariable "+")
              (EInteger 5)
              (ELet NonRecursive [LetDefinition ("k", EInteger 5)] (EVariable "k"))
            )
          ]
        )
      )
    , ( "indirect right application of match expression"
      , [qqRawCode|
                  f = 5 + (match $C{1;0} with <1> -> 5)
        |]
      , Right
        ( Program
          [ Supercombinator
            ( "f"
            , []
            , EApplication2
              (EVariable "+")
              (EInteger 5)
              (EMatch (EConstructor 1 0) [MatchCase (1, [], EInteger 5)])
            )
          ]
        )
      )
    , ( "direct right application of let expression"
      , [qqRawCode|
                  f = 5 + let k = 5 in k
        |]
      , Left
        (errFancy 8 (fancy (ErrorFail "keyword \"let\" cannot be an identifier")))
      )
    , ( "direct right application of match expression"
      , [qqRawCode|
                  f = 5 + match $C{1;0} with <1> -> 5
        |]
      , Left
        (errFancy 8 (fancy (ErrorFail "keyword \"match\" cannot be an identifier")))
      )
    ]
