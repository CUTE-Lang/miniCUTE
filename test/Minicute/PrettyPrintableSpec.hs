{-# LANGUAGE QuasiQuotes #-}
module Minicute.PrettyPrintableSpec
  ( spec
  ) where

import Test.Hspec
import Test.Hspec.Megaparsec

import Control.Monad
import Data.Either
import Minicute.Types.Minicute.Program
import Text.Megaparsec
import Text.InterpolatedString.QM

import qualified Minicute.Data.PrintSequence as PS
import qualified Minicute.PrettyPrintable as PP
import qualified Minicute.Parser.Parser as P

spec :: Spec
spec = do
  describe "prettyPrint" $ do
    forM_ testCases (uncurry programLTest)

programLTest :: TestName -> TestContent -> SpecWith (Arg Expectation)
programLTest name programString = do
  describe ("with " <> name) $ do
    it "prints re-parsable text" $ do
      program <- parseProgramL programString
      parse P.programL "" (PS.toString (PP.prettyPrint program)) `shouldParse` program
    it "prints expected text" $ do
      program <- parseProgramL programString
      PS.toString (PP.prettyPrint program) `shouldBe` programString
  where
    parseProgramL :: String -> IO MainProgramL
    parseProgramL ps = do
      parse P.programL "" ps `shouldSatisfy` isRight
      case parse P.programL "" ps of
        Right program -> return program
        Left e -> error (errorBundlePretty e)

type TestName = String
type TestContent = String
type TestCase = (TestName, TestContent)

testCases :: [TestCase]
testCases
  = [ ( "empty program"
      , [qnb||]
      )
    , ( "simple program"
      , [qnb|
             f = 5
        |]
      )
    , ( "program with multiple top-level definitions"
      , [qnb|
             f = 5;
             g = 5
        |]
      )
    , ( "program with top-level definitions with arguments"
      , [qnb|
             f x = g x 5;
             g x y = x y
        |]
      )
    , ( "program with arithmetic operators"
      , [qnb|
             f = 5 + 4
        |]
      )
    , ( "program with multiple arithmetic operators0"
      , [qnb|
             f = 5 + 4 * 5
        |]
      )
    , ( "program with multiple arithmetic operators1"
      , [qnb|
             f = (5 + 4) * 5
        |]
      )
    , ( "program with multiple arithmetic operators2"
      , [qnb|
             f = 5 - 4 - 3
        |]
      )
    , ( "program with multiple arithmetic operators3"
      , [qnb|
             f = 5 - (4 - 3)
        |]
      )
    , ( "program with let"
      , [qnb|
             f = let x = 5 in x
        |]
      )
    ]
