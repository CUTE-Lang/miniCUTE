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
import Minicute.Types.Minicute.Program
import Minicute.Utils.TH
import Text.Megaparsec

import qualified Minicute.Parser.Minicute.Parser as P

spec :: Spec
spec = do
  describe "mainProgramL parser" $ do
    forM_ mainProgramLTestCases mainProgramLTest

mainProgramLTest :: MainProgramLTestCase -> SpecWith (Arg Expectation)
mainProgramLTest (name, content, Right result) = do
  it ("parses " <> name <> " successfully") $ do
    parse P.mainProgramL "" content `shouldParse` result
mainProgramLTest (name, content, Left parseError) = do
  it ("fails to parse " <> name) $ do
    parse P.mainProgramL "" content `shouldFailWith` parseError

type TestName = String
type TestContent = String
type MainProgramLTestResult = Either (ParseError String Void) MainProgramL
type MainProgramLTestCase = (TestName, TestContent, MainProgramLTestResult)

mainProgramLTestCases :: [MainProgramLTestCase]
mainProgramLTestCases
  = simpleMainProgramLTestCases
    <> arithmeticOperatorMainProgramLTestCases
    <> constructorMainProgramLTestCases
    <> applicationMainProgramLTestCases
    <> supercombinatorMainProgramLTestCases
    <> letAndLetrecMainProgramLTestCases
    <> matchMainProgramLTestCases
    <> lambdaMainProgramLTestCases
    <> complexMainProgramLTestCases
  where
    simpleMainProgramLTestCases
      = tupleUnzip2 <$> zip simpleLabels simpleMainProgramLTestTemplates
    simpleLabels = ("simple case" <>) . show <$> [0..]

simpleMainProgramLTestTemplates :: [(TestContent, MainProgramLTestResult)]
simpleMainProgramLTestTemplates
  = [ ( [qqRawCode||]
      , Right
        ( ProgramL
          [
          ]
        )
      )
    , ( [qqRawCode|
                  f = 1
        |]
      , Right
        ( ProgramL
          [ SupercombinatorL
            "f"
            []
            (ELInteger 1)
          ]
        )
      )
    , ( [qqRawCode|
                  f = 1;
        |]
      , Right
        ( ProgramL
          [ SupercombinatorL
            "f"
            []
            (ELInteger 1)
          ]
        )
      )
    , ( [qqRawCode|
                  f=1;
        |]
      , Right
        ( ProgramL
          [ SupercombinatorL
            "f"
            []
            (ELInteger 1)
          ]
        )
      )
    , ( [qqRawCode|
                  f= 1;
        |]
      , Right
        ( ProgramL
          [ SupercombinatorL
            "f"
            []
            (ELInteger 1)
          ]
        )
      )
    , ( [qqRawCode|
                  f= 1 ;
        |]
      , Right
        ( ProgramL
          [ SupercombinatorL
            "f"
            []
            (ELInteger 1)
          ]
        )
      )
    , ( [qqRawCode|
                  f = 1;
                  g = 2
        |]
      , Right
        ( ProgramL
          [ SupercombinatorL
            "f"
            []
            (ELInteger 1)
          , SupercombinatorL
            "g"
            []
            (ELInteger 2)
          ]
        )
      )
    , ( [qqRawCode|
                  f = 1  ;
                  g=2 ;
        |]
      , Right
        ( ProgramL
          [ SupercombinatorL
            "f"
            []
            (ELInteger 1)
          , SupercombinatorL
            "g"
            []
            (ELInteger 2)
          ]
        )
      )
    , ( [qqRawCode|
                  f = g;
                  g = 2
        |]
      , Right
        ( ProgramL
          [ SupercombinatorL
            "f"
            []
            (ELVariable "g")
          , SupercombinatorL
            "g"
            []
            (ELInteger 2)
          ]
        )
      )
    , ( [qqRawCode|
                  matchx = matchx
        |]
      , Right
        ( ProgramL
          [ SupercombinatorL
            "matchx"
            []
            (ELVariable "matchx")
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

arithmeticOperatorMainProgramLTestCases :: [MainProgramLTestCase]
arithmeticOperatorMainProgramLTestCases
  = [ ( "addition of two numbers"
      , [qqRawCode|
                  f = 1 + 1
        |]
      , Right
        ( ProgramL
          [ SupercombinatorL
            "f"
            []
            (ELApplication2 (ELVariable "+") (ELInteger 1) (ELInteger 1))
          ]
        )
      )
    , ( "addition of a number and a variable"
      , [qqRawCode|
                  f = 1 * g;
                  g = 3
        |]
      , Right
        ( ProgramL
          [ SupercombinatorL
            "f"
            []
            (ELApplication2 (ELVariable "*") (ELInteger 1) (ELVariable "g"))
          , SupercombinatorL
            "g"
            []
            (ELInteger 3)
          ]
        )
      )
    , ( "multiple addition of numbers"
      , [qqRawCode|
                  f = 1 + (3 + 4)
        |]
      , Right
        ( ProgramL
          [ SupercombinatorL
            "f"
            []
            ( ELApplication2
              (ELVariable "+")
              (ELInteger 1)
              (ELApplication2 (ELVariable "+") (ELInteger 3) (ELInteger 4))
            )
          ]
        )
      )
    , ( "operator association of -"
      , [qqRawCode|
                  f = 3 - 2 - 1
        |]
      , Right
        ( ProgramL
          [ SupercombinatorL
            "f"
            []
            ( ELApplication2
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
      , [qqRawCode|
                  f = 1 * 2 + 3
        |]
      , Right
        ( ProgramL
          [ SupercombinatorL
            "f"
            []
            ( ELApplication2
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

constructorMainProgramLTestCases :: [MainProgramLTestCase]
constructorMainProgramLTestCases
  = [ ( "basic constructor"
      , [qqRawCode|
                  f = $C{1;0};
                  g = $C{2;2}
        |]
      , Right
        ( ProgramL
          [ SupercombinatorL
            "f"
            []
            (ELConstructor 1 0)
          , SupercombinatorL
            "g"
            []
            (ELConstructor 2 2)
          ]
        )
      )
    , ( "constructor with arguments"
      , [qqRawCode|
                  f = $C{1;1} 5;
                  g = $C{2;3} f
        |]
      , Right
        ( ProgramL
          [ SupercombinatorL
            "f"
            []
            (ELApplication (ELConstructor 1 1) (ELInteger 5))
          , SupercombinatorL
            "g"
            []
            (ELApplication (ELConstructor 2 3) (ELVariable "f"))
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

applicationMainProgramLTestCases :: [MainProgramLTestCase]
applicationMainProgramLTestCases
  = [ ( "application of an integer"
      , [qqRawCode|
                  f = g 5
        |]
      , Right
        ( ProgramL
          [ SupercombinatorL
            "f"
            []
            (ELApplication (ELVariable "g") (ELInteger 5))
          ]
        )
      )
    , ( "application of a variable"
      , [qqRawCode|
                  f = g f
        |]
      , Right
        ( ProgramL
          [ SupercombinatorL
            "f"
            []
            (ELApplication (ELVariable "g") (ELVariable "f"))
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

supercombinatorMainProgramLTestCases :: [MainProgramLTestCase]
supercombinatorMainProgramLTestCases
  = [ ( "supercombinator with an argument"
      , [qqRawCode|
                  f x = x
        |]
      , Right
        ( ProgramL
          [ SupercombinatorL
            "f"
            ["x"]
            (ELVariable "x")
          ]
        )
      )
    , ( "supercombinator with two argument"
      , [qqRawCode|
                  f x y = x y
        |]
      , Right
        ( ProgramL
          [ SupercombinatorL
            "f"
            ["x", "y"]
            (ELApplication (ELVariable "x") (ELVariable "y"))
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

letAndLetrecMainProgramLTestCases :: [MainProgramLTestCase]
letAndLetrecMainProgramLTestCases
  = [ ( "let with a single definition"
      , [qqRawCode|
                  f = let x = 5 in x
        |]
      , Right
        ( ProgramL
          [ SupercombinatorL
            "f"
            []
            ( ELLet
              NonRecursive
              [ LetDefinitionL "x" (ELInteger 5)
              ]
              (ELVariable "x")
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
        ( ProgramL
          [ SupercombinatorL
            "f"
            []
            ( ELLet
              Recursive
              [ LetDefinitionL "x" (ELInteger 5)
              ]
              (ELVariable "x")
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
        ( ProgramL
          [ SupercombinatorL
            "f"
            []
            ( ELLet
              NonRecursive
              [ LetDefinitionL "x" (ELInteger 5)
              , LetDefinitionL "y" (ELInteger 4)
              ]
              (ELApplication2 (ELVariable "+") (ELVariable "x") (ELVariable "y"))
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
        ( ProgramL
          [ SupercombinatorL
            "f"
            []
            ( ELLet
              Recursive
              [ LetDefinitionL "x" (ELInteger 5)
              , LetDefinitionL "y" (ELApplication2 (ELVariable "+") (ELVariable "x") (ELVariable "x"))
              , LetDefinitionL "z" (ELApplication2 (ELVariable "*") (ELVariable "x") (ELVariable "y"))
              ]
              (ELVariable "z")
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
        ( ProgramL
          [ SupercombinatorL
            "f"
            []
            ( ELLet
              NonRecursive
              [ LetDefinitionL "x" (ELLet NonRecursive [ LetDefinitionL "k" (ELInteger 5) ] (ELVariable "k"))
              ]
              (ELVariable "x")
            )
          ]
        )
      )
    , ( "let with nested letrec"
      , [qqRawCode|
                  f = let x = letrec k = 5 in k; in x
        |]
      , Right
        ( ProgramL
          [ SupercombinatorL
            "f"
            []
            ( ELLet
              NonRecursive
              [ LetDefinitionL "x" (ELLet Recursive [ LetDefinitionL "k" (ELInteger 5) ] (ELVariable "k"))
              ]
              (ELVariable "x")
            )
          ]
        )
      )
    , ( "letrec with nested let"
      , [qqRawCode|
                  f = letrec x = let k = 5; in k in x
        |]
      , Right
        ( ProgramL
          [ SupercombinatorL
            "f"
            []
            ( ELLet
              Recursive
              [ LetDefinitionL "x" (ELLet NonRecursive [ LetDefinitionL "k" (ELInteger 5) ] (ELVariable "k"))
              ]
              (ELVariable "x")
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
        ( ProgramL
          [ SupercombinatorL
            "f"
            []
            ( ELLet
              Recursive
              [ LetDefinitionL "x" (ELLet Recursive [ LetDefinitionL "k" (ELInteger 5) ] (ELVariable "k"))
              ]
              (ELVariable "x")
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

matchMainProgramLTestCases :: [MainProgramLTestCase]
matchMainProgramLTestCases
  = [ ( "match with a single match case"
      , [qqRawCode|
                  f = match $C{1;0} with <1> -> 5
        |]
      , Right
        ( ProgramL
          [ SupercombinatorL
            "f"
            []
            ( ELMatch
              (ELConstructor 1 0)
              [ MatchCaseL 1 [] (ELInteger 5)
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
        ( ProgramL
          [ SupercombinatorL
            "f"
            []
            ( ELMatch
              (ELConstructor 2 0)
              [ MatchCaseL 1 [] (ELInteger 5)
              , MatchCaseL 2 [] (ELInteger 3)
              , MatchCaseL 4 [] (ELVariable "g")
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
        ( ProgramL
          [ SupercombinatorL
            "f"
            []
            ( ELMatch
              (ELApplication2 (ELConstructor 2 2) (ELInteger 5) (ELInteger 4))
              [ MatchCaseL 1 ["x", "y"] (ELVariable "x")
              , MatchCaseL 2 ["a", "b"] (ELVariable "b")
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
        ( ProgramL
          [ SupercombinatorL
            "f"
            []
            ( ELMatch
              (ELApplication2 (ELConstructor 2 2) (ELInteger 5) (ELInteger 4))
              [ MatchCaseL 1 ["x", "y"] (ELVariable "x")
              , MatchCaseL 2 ["a", "b"] (ELVariable "b")
              ]
            )
          , SupercombinatorL
            "g"
            []
            (ELInteger 1)
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

lambdaMainProgramLTestCases :: [MainProgramLTestCase]
lambdaMainProgramLTestCases
  = [ ( "lambda with a single argument"
      , [qqRawCode|
                  f = \x -> x
        |]
      , Right
        ( ProgramL
          [ SupercombinatorL
            "f"
            []
            ( ELLambda
              ["x"]
              (ELVariable "x")
            )
          ]
        )
      )
    , ( "lambda with multiple arguments"
      , [qqRawCode|
                  f = \x y -> x + y
        |]
      , Right
        ( ProgramL
          [ SupercombinatorL
            "f"
            []
            ( ELLambda
              ["x", "y"]
              (ELApplication2 (ELVariable "+") (ELVariable "x") (ELVariable "y"))
            )
          ]
        )
      )
    , ( "lambda with nested lambda"
      , [qqRawCode|
                  f = \x -> \y -> x + y
        |]
      , Right
        ( ProgramL
          [ SupercombinatorL
            "f"
            []
            ( ELLambda
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
      , [qqRawCode|
                  f = (\x -> x) 5
        |]
      , Right
        ( ProgramL
          [ SupercombinatorL
            "f"
            []
            ( ELApplication
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

complexMainProgramLTestCases :: [MainProgramLTestCase]
complexMainProgramLTestCases
  = [ ( "indirect right application of let expression"
      , [qqRawCode|
                  f = 5 + (let k = 5 in k)
        |]
      , Right
        ( ProgramL
          [ SupercombinatorL
            "f"
            []
            ( ELApplication2
              (ELVariable "+")
              (ELInteger 5)
              (ELLet NonRecursive [LetDefinitionL "k" (ELInteger 5)] (ELVariable "k"))
            )
          ]
        )
      )
    , ( "indirect right application of match expression"
      , [qqRawCode|
                  f = 5 + (match $C{1;0} with <1> -> 5)
        |]
      , Right
        ( ProgramL
          [ SupercombinatorL
            "f"
            []
            ( ELApplication2
              (ELVariable "+")
              (ELInteger 5)
              (ELMatch (ELConstructor 1 0) [MatchCaseL 1 [] (ELInteger 5)])
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