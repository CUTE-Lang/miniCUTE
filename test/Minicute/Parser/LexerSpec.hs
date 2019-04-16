{-# LANGUAGE BinaryLiterals #-}
module Minicute.Parser.LexerSpec
  ( spec
  ) where

import Test.Hspec
import Test.Hspec.Megaparsec

import qualified Minicute.Parser.Lexer as L
import Text.Megaparsec

-- TODO: Update these tests to use QuickCheck
spec :: Spec
spec
  = do
  describe "integer lexer" $ do
    describe "when an input is not prefixed" $ do
      it "parses a decimal number with a non-zero start" $ do
        parse L.integer "" "10" `shouldParse` 10
      it "does not parse a decimal number with the zero start" $ do
        runParser' L.integer (initialState "010") `failsLeaving` "10"

    describe "when an input is prefixed" $ do
      it "parses a binary number with a non-zero start" $ do
        parse L.integer "" "0B1101" `shouldParse` 0b1101
        parse L.integer "" "0b1101" `shouldParse` 0b1101
      it "parses a binary number with the zero start" $ do
        parse L.integer "" "0B01011" `shouldParse` 0b1011
        parse L.integer "" "0b01011" `shouldParse` 0b1011
      it "parses a octal number with a non-zero start" $ do
        parse L.integer "" "0O777" `shouldParse` 0o777
        parse L.integer "" "0o1262" `shouldParse` 0o1262
      it "parses a octal number with the zero start" $ do
        parse L.integer "" "0O0777" `shouldParse` 0o777
        parse L.integer "" "0o00111" `shouldParse` 0o111
      it "parses a decimal number with a non-zero start" $ do
        parse L.integer "" "0D102" `shouldParse` 102
        parse L.integer "" "0d52" `shouldParse` 52
      it "parses a decimal number with the zero start" $ do
        parse L.integer "" "0D0304" `shouldParse` 304
        parse L.integer "" "0d0992" `shouldParse` 992
      it "parses a hexadecimal number with a non-zero start" $ do
        parse L.integer "" "0Xaba" `shouldParse` 0xaba
        parse L.integer "" "0xAbD" `shouldParse` 0xabd
      it "parses a hexadecimal number with the zero start" $ do
        parse L.integer "" "0X0Ff5" `shouldParse` 0xff5
        parse L.integer "" "0x055E" `shouldParse` 0x55e

    describe "when an input has trailing spaces" $ do
      it "parses successfully" $ do
        parse L.integer "" "10  " `shouldParse` 10
        parse L.integer "" "0b11\n" `shouldParse` 0b11
        parse L.integer "" "0o1252\t" `shouldParse` 0o1252
        parse L.integer "" "0d521 \t\n" `shouldParse` 521
        parse L.integer "" "0xfcab242\t\t  \t\t" `shouldParse` 0xfcab242

    describe "when an input has preceding spaces" $ do
      it "does not parse successfully" $ do
        runParser' L.integer (initialState "\t12") `failsLeaving` "\t12"
        runParser' L.integer (initialState "\n0b101") `failsLeaving` "\n0b101"
        runParser' L.integer (initialState " 0xfcab242") `failsLeaving` " 0xfcab242"
