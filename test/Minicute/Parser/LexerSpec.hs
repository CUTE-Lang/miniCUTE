{-# LANGUAGE BinaryLiterals #-}
module Minicute.Parser.LexerSpec
  ( spec
  ) where

import Minicute.Parser.TestUtils
import Test.Hspec

import Data.Either
import qualified Minicute.Parser.Lexer as L

-- TODO: Update these tests to use QuickCheck
spec :: Spec
spec = do
  describe "integer lexer" $ do
    describe "when an input is not prefixed" $ do
      it "parses a decimal number with a non-zero start" $ do
        runParserTest L.integer "10" `shouldBe` Right 10
      it "does not parse a decimal number with the zero start" $ do
        runParserTest L.integer "010" `shouldSatisfy` isLeft

    describe "when an input is prefixed" $ do
      it "parses a binary number with a non-zero start" $ do
        runParserTest L.integer "0B1101" `shouldBe` Right 0b1101
        runParserTest L.integer "0b1101" `shouldBe` Right 0b1101
      it "parses a binary number with the zero start" $ do
        runParserTest L.integer "0B01011" `shouldBe` Right 0b1011
        runParserTest L.integer "0b01011" `shouldBe` Right 0b1011
      it "parses a octal number with a non-zero start" $ do
        runParserTest L.integer "0O777" `shouldBe` Right 0o777
        runParserTest L.integer "0o1262" `shouldBe` Right 0o1262
      it "parses a octal number with the zero start" $ do
        runParserTest L.integer "0O0777" `shouldBe` Right 0o777
        runParserTest L.integer "0o00111" `shouldBe` Right 0o111
      it "parses a decimal number with a non-zero start" $ do
        runParserTest L.integer "0D102" `shouldBe` Right 102
        runParserTest L.integer "0d52" `shouldBe` Right 52
      it "parses a decimal number with the zero start" $ do
        runParserTest L.integer "0D0304" `shouldBe` Right 304
        runParserTest L.integer "0d0992" `shouldBe` Right 992
      it "parses a hexadecimal number with a non-zero start" $ do
        runParserTest L.integer "0Xaba" `shouldBe` Right 0xaba
        runParserTest L.integer "0xAbD" `shouldBe` Right 0xabd
      it "parses a hexadecimal number with the zero start" $ do
        runParserTest L.integer "0X0Ff5" `shouldBe` Right 0xff5
        runParserTest L.integer "0x055E" `shouldBe` Right 0x55e

    describe "when an input has trailing spaces" $ do
      it "parses successfully" $ do
        runParserTest L.integer "10  " `shouldBe` Right 10
        runParserTest L.integer "0b11\n" `shouldBe` Right 0b11
        runParserTest L.integer "0o1252\t" `shouldBe` Right 0o1252
        runParserTest L.integer "0d521 \t\n" `shouldBe` Right 521
        runParserTest L.integer "0xfcab242\t\t  \t\t" `shouldBe` Right 0xfcab242

    describe "when an input has preceding spaces" $ do
      it "does not parse successfully" $ do
        runParserTest L.integer "\t12" `shouldSatisfy` isLeft
        runParserTest L.integer "\n0b101" `shouldSatisfy` isLeft
        runParserTest L.integer " 0xfcab242" `shouldSatisfy` isLeft
