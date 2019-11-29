{- HLINT ignore "Redundant do" -}
{-# LANGUAGE QuasiQuotes #-}
-- |
-- Copyright: (c) 2018-present Junyoung Clare Jang
-- License: BSD 3-Clause
module Minicute.PrettySpec
  ( spec_prettyPrint
  ) where

import Test.Hspec.Megaparsec
import Test.Tasty.Hspec

import Control.Monad
import Data.Either
import Minicute.Data.Minicute.Program
import Minicute.Utils.Common.TH
import Text.Megaparsec

import qualified Data.Text.Prettyprint.Doc.Minicute as PP
import qualified Minicute.Parser.Minicute.Parser as P

spec_prettyPrint :: Spec
spec_prettyPrint
  = forM_ testCases (uncurry programMCTest)

programMCTest :: TestName -> TestContent -> SpecWith (Arg Expectation)
programMCTest name programString = do
  describe ("of " <> name) $ do
    it "prints re-parsable text" $ do
      program <- parseProgramMC programString
      parse P.mainProgramMC "" (show (PP.prettyMC0 program)) `shouldParse` program
    it "prints expected text" $ do
      program <- parseProgramMC programString
      show (PP.prettyMC0 program) `shouldBe` programString
  where
    parseProgramMC :: String -> IO MainProgramMC
    parseProgramMC ps = do
      parse P.mainProgramMC "" ps `shouldSatisfy` isRight
      case parse P.mainProgramMC "" ps of
        Right program -> pure program
        Left e -> error (errorBundlePretty e)

type TestName = String
type TestContent = String
type TestCase = (TestName, TestContent)

testCases :: [TestCase]
testCases
  = [ ( "empty program"
      , [qqRawCode||]
      )
    , ( "simple program"
      , [qqRawCode|
                  f = 5
        |]
      )
    , ( "program with multiple top-level definitions"
      , [qqRawCode|
                  f = 5;
                  g = 5
        |]
      )
    , ( "program with top-level definitions with arguments"
      , [qqRawCode|
                  f x = g x 5;
                  g x y = x y
        |]
      )
    , ( "program with arithmetic operators"
      , [qqRawCode|
                  f = 5 + 4
        |]
      )
    , ( "program with multiple arithmetic operators0"
      , [qqRawCode|
                  f = 5 + 4 * 5
        |]
      )
    , ( "program with multiple arithmetic operators1"
      , [qqRawCode|
                  f = (5 + 4) * 5
        |]
      )
    , ( "program with multiple arithmetic operators2"
      , [qqRawCode|
                  f = 5 - 4 - 3
        |]
      )
    , ( "program with multiple arithmetic operators3"
      , [qqRawCode|
                  f = 5 - (4 - 3)
        |]
      )
    , ( "program with a let definition"
      , [qqRawCode|
                  f = let
                        x = 5
                      in
                        x
        |]
      )
    , ( "program with let definitions"
      , [qqRawCode|
                  f = let
                        x = 5;
                        y = 4
                      in
                        g x y
        |]
      )
    , ( "program with match"
      , [qqRawCode|
                  f = match x with
                        <1> -> 1;
                        <2> -> 2
        |]
      )
    , ( "program with lambda"
      , [qqRawCode|
                  f = \x ->
                        x + 4
        |]
      )
    , ( "program with lambda of multilined body"
      , [qqRawCode|
                  f = \x ->
                        let
                          y = 4
                        in
                          x + y
        |]
      )
    , ( "program with an application with lambda"
      , [qqRawCode|
                  f = g (\x ->
                           x + 4)
        |]
      )
    , ( "program with an application for lambda"
      , [qqRawCode|
                  f = (\x ->
                         x + 4) (5 * 3)
        |]
      )
    ]
