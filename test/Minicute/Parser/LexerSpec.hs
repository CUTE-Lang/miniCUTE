{- HLINT ignore "Redundant do" -}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Minicute.Parser.LexerSpec
  ( spec
  ) where

import Test.Hspec
import Test.Hspec.Megaparsec

import Minicute.Parser.Types ( Parser )
import Text.Megaparsec

import qualified Minicute.Parser.Lexer as L

-- |
-- __TODO: Update these tests to use QuickCheck__
spec :: Spec
spec = do
  describe "integer lexer" $ do
    describe "when an input is not prefixed" $ do
      it "parses a zero" $ do
        (L.integer, "0") `shouldJustParseInto` 0
      it "parses a decimal number with a non-zero start" $ do
        (L.integer, "10") `shouldJustParseInto` 10
      it "fails to parse a decimal number with the zero start and leaves from the problematic character" $ do
        (L.integer, "010") `failsLeavingInput` "10"

    describe "when an input is prefixed" $ do
      it "parses a binary number with a non-zero start" $ do
        (L.integer, "0B1101") `shouldJustParseInto` 0b1101
        (L.integer, "0b1101") `shouldJustParseInto` 0b1101
      it "parses a binary number with the zero start" $ do
        (L.integer, "0B01011") `shouldJustParseInto` 0b1011
        (L.integer, "0b01011") `shouldJustParseInto` 0b1011
      it "parses a octal number with a non-zero start" $ do
        (L.integer, "0O777") `shouldJustParseInto` 0o777
        (L.integer, "0o1262") `shouldJustParseInto` 0o1262
      it "parses a octal number with the zero start" $ do
        (L.integer, "0O0777") `shouldJustParseInto` 0o777
        (L.integer, "0o00111") `shouldJustParseInto` 0o111
      it "parses a decimal number with a non-zero start" $ do
        (L.integer, "0D102") `shouldJustParseInto` 102
        (L.integer, "0d52") `shouldJustParseInto` 52
      it "parses a decimal number with the zero start" $ do
        (L.integer, "0D0304") `shouldJustParseInto` 304
        (L.integer, "0d0992") `shouldJustParseInto` 992
      it "parses a hexadecimal number with a non-zero start" $ do
        (L.integer, "0Xaba") `shouldJustParseInto` 0xaba
        (L.integer, "0xAbD") `shouldJustParseInto` 0xabd
      it "parses a hexadecimal number with the zero start" $ do
        (L.integer, "0X0Ff5") `shouldJustParseInto` 0xff5
        (L.integer, "0x055E") `shouldJustParseInto` 0x55e

    describe "when an input has trailing spaces" $ do
      it "parses a integer" $ do
        (L.integer, "10  ") `shouldJustParseInto` 10
        (L.integer, "0b11\n") `shouldJustParseInto` 0b11
        (L.integer, "0o1252\t") `shouldJustParseInto` 0o1252
        (L.integer, "0d521 \t\n") `shouldJustParseInto` 521
        (L.integer, "0xfcab242\t\t  \t\t") `shouldJustParseInto` 0xfcab242

    describe "when an input has preceding spaces" $ do
      it "fails to parse and leaves the original input" $ do
        (L.integer, "\t12") `failsLeavingInput` "\t12"
        (L.integer, "\n0b101") `failsLeavingInput` "\n0b101"
        (L.integer, " 0xfcab242") `failsLeavingInput` " 0xfcab242"

    describe "when an input has trailing alphabets" $ do
      it "fails to parse and leaves from the problematic character" $ do
        (L.integer, "0k") `failsLeavingInput` "k"
        (L.integer, "50a") `failsLeavingInput` "a"
        (L.integer, "204bjl") `failsLeavingInput` "bjl"
        (L.integer, "0b1101pqw") `failsLeavingInput` "pqw"
        (L.integer, "0X202PG") `failsLeavingInput` "PG"

  describe "identifier lexer" $ do
    describe "when an input has alphabets only" $ do
      it "parses an identifier" $ do
        (L.identifier, "abc") `shouldJustParseInto` "abc"
        (L.identifier, "fwehlg") `shouldJustParseInto` "fwehlg"
        (L.identifier, "Qpozmgdpxsc") `shouldJustParseInto` "Qpozmgdpxsc"

    describe "when an input has alphanumeric characters starting with an alphabet" $ do
      it "parses an identifier" $ do
        (L.identifier, "abc40ias") `shouldJustParseInto` "abc40ias"
        (L.identifier, "fwehlg120ad") `shouldJustParseInto` "fwehlg120ad"
        (L.identifier, "Gew129g01f") `shouldJustParseInto` "Gew129g01f"

    describe "when an input has alphanumeric characters starting with a number" $ do
      it "fails to parse and leaves the original input" $ do
        (L.identifier, "5abc") `failsLeavingInput` "5abc"
        (L.identifier, "23gew") `failsLeavingInput` "23gew"
        (L.identifier, "0Pq") `failsLeavingInput` "0Pq"

    describe "when an input has alphanumeric characters and an _ starting with an alphabet or an _" $ do
      it "parses an identifier" $ do
        (L.identifier, "abc_13_") `shouldJustParseInto` "abc_13_"
        (L.identifier, "_Jo") `shouldJustParseInto` "_Jo"
        (L.identifier, "_0Pq") `shouldJustParseInto` "_0Pq"

    describe "when an input has alphanumeric characters and _ starting with a number" $ do
      it "fails to parse and leaves the original input" $ do
        (L.identifier, "4abc_13_") `failsLeavingInput` "4abc_13_"
        (L.identifier, "56_Jo") `failsLeavingInput` "56_Jo"
        (L.identifier, "04_12") `failsLeavingInput` "04_12"

shouldParseInto :: (Show a, Eq a) => (Parser a, String) -> a -> String -> IO ()
shouldParseInto (p, content) value leaving = do
  parse p "" content `shouldParse` value
  runParser' p (initialState content) `succeedsLeaving` leaving

shouldJustParseInto :: (Show a, Eq a) => (Parser a, String) -> a -> IO ()
shouldJustParseInto pair value = shouldParseInto pair value ""

failsLeavingInput :: (Show a, Eq a) => (Parser a, String) -> String -> IO ()
failsLeavingInput (p, content) leaving = do
  runParser' p (initialState content) `failsLeaving` leaving
