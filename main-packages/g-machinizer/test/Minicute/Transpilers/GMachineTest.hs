{- HLINT ignore "Redundant do" -}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
-- |
-- Copyright: (c) 2018-present Junyoung Clare Jang
-- License: BSD 3-Clause
module Minicute.Transpilers.GMachineTest
  ( spec_transpileProgram
  ) where

import Test.Tasty.Hspec

import Control.Monad
import Data.Tuple.Extra
import Minicute.Transpilers.GMachine
import Minicute.Utils.GMachine.TH
import Minicute.Utils.Minicute.TH

spec_transpileProgram :: Spec
spec_transpileProgram = forM_ testCases (uncurry3 transpileProgramTest)

transpileProgramTest :: TestName -> TestBeforeContent -> TestAfterContent -> SpecWith (Arg Expectation)
transpileProgramTest name beforeContent afterContent = do
  it ("transpile " <> name <> " into valid GMachine program") $ do
    transpileProgram beforeContent `shouldBe` afterContent

type TestName = String
type TestBeforeContent = MainProgramLLMC
type TestAfterContent = GMachineProgram
type TestCase = (TestName, TestBeforeContent, TestAfterContent)

testCases :: [TestCase]
testCases
  = [ ( "empty program"
      , [qqMiniMainLLMC|
        |]
      , [qqGMachine|
        |]
      )

    , ( "program with constant top-level definition"
      , [qqMiniMainLLMC|
                   f = 5;
                   g = $C{9;0}
        |]
      , [qqGMachine|
                   f<0> {
                     PushBasicValue 5;
                     UpdateAsInteger 0;
                     Return;
                   }
                   g<0> {
                     PushBasicValue 9;
                     UpdateAsStructure 0;
                     Return;
                   }
        |]
      )

    , ( "program with an argument"
      , [qqMiniMainLLMC|
                   f x = x;
                   g x = $C{3;0};
        |]
      , [qqGMachine|
                   f<1> {
                     Copy 0;
                     Eval;
                     Update 2;
                     Pop 2;
                     Unwind;
                   }
                   g<1> {
                     PushBasicValue 3;
                     UpdateAsStructure 1;
                     Return;
                   }
        |]
      )

    , ( "program with arguments"
      , [qqMiniMainLLMC|
                   f x y = x;
                   g a b c = b
        |]
      , [qqGMachine|
                   f<2> {
                     Copy 0;
                     Eval;
                     Update 3;
                     Pop 3;
                     Unwind;
                   }
                   g<3> {
                     Copy 1;
                     Eval;
                     Update 4;
                     Pop 4;
                     Unwind;
                   }
        |]
      )

    , ( "program with a simple application"
      , [qqMiniMainLLMC|
                   f = g 4;
                   g x = x;
        |]
      , [qqGMachine|
                   f<0> {
                     MakeGlobal g;
                     MakeInteger 4;
                     MakeApplication;
                     Eval;
                     Update 1;
                     Pop 1;
                     Unwind;
                   }
                   g<1> {
                     Copy 0;
                     Eval;
                     Update 2;
                     Pop 2;
                     Unwind;
                   }
        |]
      )

    , ( "program with a constructor application"
      , [qqMiniMainLLMC|
                   f = $C{1;1} 4;
                   g x = $C{3;2} x
        |]
      , [qqGMachine|
                   f<0> {
                     MakeConstructor 1 1;
                     MakeInteger 4;
                     MakeApplication;
                     Eval;
                     Update 1;
                     Pop 1;
                     Unwind;
                   }
                   g<1> {
                     MakeConstructor 3 2;
                     Copy 1;
                     MakeApplication;
                     Eval;
                     Update 2;
                     Pop 2;
                     Unwind;
                   }
        |]
      )

    , ( "program with an arithmetic operation"
      , [qqMiniMainLLMC|
                   f = 2 + 3
        |]
      , [qqGMachine|
                   f<0> {
                     PushBasicValue 2;
                     PushBasicValue 3;
                     Primitive +;
                     UpdateAsInteger 0;
                     Return;
                   }
        |]
      )

    , ( "program with multiple arithmetic operations"
      , [qqMiniMainLLMC|
                   f = 2 + 3 * 4 + 7
        |]
      , [qqGMachine|
                   f<0> {
                       PushBasicValue 2;
                         PushBasicValue 3;
                         PushBasicValue 4;
                         Primitive *;
                       Primitive +;
                     PushBasicValue 7;
                     Primitive +;
                     UpdateAsInteger 0;
                     Return;
                   }
        |]
      )

    , ( "program with an arithmetic operation in an application"
      , [qqMiniMainLLMC|
                   f = g (3 * 4);
                   g x = x;
        |]
      , [qqGMachine|
                   f<0> {
                     MakeGlobal g;
                         MakeGlobal *;
                         MakeInteger 3;
                         MakeApplication;
                       MakeInteger 4;
                       MakeApplication;
                     MakeApplication;
                     Eval;
                     Update 1;
                     Pop 1;
                     Unwind;
                   }
                   g<1> {
                     Copy 0;
                     Eval;
                     Update 2;
                     Pop 2;
                     Unwind;
                   }
        |]
      )

    , ( "program with a let expression returning an integer"
      , [qqMiniMainLLMC|
                   f = let
                         x = 4
                       in
                         3
        |]
      , [qqGMachine|
                   f<0> {
                     MakeInteger 4;
                     PushBasicValue 3;
                     UpdateAsInteger 1;
                     Return;
                   }
        |]
      )

    , ( "program with a let expression of a definition"
      , [qqMiniMainLLMC|
                   f = let
                         x = 4
                       in
                         x
        |]
      , [qqGMachine|
                   f<0> {
                     MakeInteger 4;
                     Copy 0;
                     Eval;
                     Update 2;
                     Pop 2;
                     Unwind;
                   }
        |]
      )

    , ( "program with a let expression of definitions"
      , [qqMiniMainLLMC|
                   f = let
                         x = 4;
                         y = 3
                       in
                         x * y
        |]
      , [qqGMachine|
                   f<0> {
                     MakeInteger 4;
                     MakeInteger 3;
                       Copy 1;
                       Eval;
                       PushExtractedValue;
                       Copy 0;
                       Eval;
                       PushExtractedValue;
                     Primitive *;
                     UpdateAsInteger 2;
                     Return;
                   }
        |]
      )

    , ( "program with a let expression in an arithmetic expression"
      , [qqMiniMainLLMC|
                   f = 5 + (let
                              x = 4
                            in
                              x)
        |]
      , [qqGMachine|
                   f<0> {
                     PushBasicValue 5;
                       MakeInteger 4;
                       Copy 0;
                       Eval;
                       PushExtractedValue;
                       Pop 1;
                     Primitive +;
                     UpdateAsInteger 0;
                     Return;
                   }
        |]
      )

    , ( "program with a letrec expression with a definition"
      , [qqMiniMainLLMC|
                   f = letrec
                         x = 4
                       in
                         x
        |]
      , [qqGMachine|
                   f<0> {
                     MakePlaceholders 1;
                     MakeInteger 4;
                     Update 1;
                     Copy 0;
                     Eval;
                     Update 2;
                     Pop 2;
                     Unwind;
                   }
        |]
      )

    , ( "program with a letrec expression with definitions"
      , [qqMiniMainLLMC|
                   f = letrec
                         x = 2 + y;
                         y = x - 3;
                       in
                         x / y
        |]
      , [qqGMachine|
                   f<0> {
                     MakePlaceholders 2;
                       MakeGlobal +;
                       MakeInteger 2;
                       MakeApplication;
                     Copy 1;
                     MakeApplication;
                     Update 2;
                       MakeGlobal -;
                       Copy 2;
                       MakeApplication;
                     MakeInteger 3;
                     MakeApplication;
                     Update 2;
                       Copy 1;
                       Eval;
                       PushExtractedValue;
                       Copy 0;
                       Eval;
                       PushExtractedValue;
                     Primitive /;
                     UpdateAsInteger 2;
                     Return;
                   }
        |]
      )

    , ( "program with a match expression 1"
      , [qqMiniMainLLMC|
                   f = match $C{1;0} with
                         <1> -> 0;
                         <2> h t -> h;
        |]
      , [qqGMachine|
                   f<0> {
                     MakeStructure 1 0;
                     Match {
                       1 ->
                         Destruct 0;
                         PushBasicValue 0;
                         UpdateAsInteger 0;
                         Return;
                       2 ->
                         Destruct 2;
                         Copy 0;
                         Eval;
                         Update 3;
                         Pop 3;
                         Unwind;
                     };
                   }
        |]
      )

    , ( "program with a match expression 2"
      , [qqMiniMainLLMC|
                   f = match $C{2;2} 2 $C{1;0} with
                         <1> -> 0;
                         <2> h t -> h;
        |]
      , [qqGMachine|
                   f<0> {
                       MakeConstructor 2 2;
                       MakeInteger 2;
                       MakeApplication;
                     MakeStructure 1 0;
                     MakeApplication;
                     Eval;
                     Match {
                       1 ->
                         Destruct 0;
                         PushBasicValue 0;
                         UpdateAsInteger 0;
                         Return;
                       2 ->
                         Destruct 2;
                         Copy 0;
                         Eval;
                         Update 3;
                         Pop 3;
                         Unwind;
                     };
                   }
        |]
      )
    ]
