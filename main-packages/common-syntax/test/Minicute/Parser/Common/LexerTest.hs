{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Use camelCase" -}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Copyright: (c) 2018-present Junyoung Clare Jang
-- License: BSD 3-Clause
module Minicute.Parser.Common.LexerTest
  ( spec_integer_lexer
  , hprop_integer_lexer_accepts_non_negative_integer_of_different_bases
  , hprop_integer_lexer_rejects_negative_integer
  , hprop_integer_lexer_rejects_strings_having_number_prefix_starting_with_zero
  , hprop_integer_lexer_rejects_strings_without_integer_prefix

  , hspec_identifier_lexer_accepts_alphabet_strings
  , hspec_identifier_lexer_accepts_alphanumeric_and_underscore_strings_starting_with_alphabet
  , hspec_identifier_lexer_accepts_alphanumeric_and_underscore_strings_starting_with_underscore
  , hspec_identifier_lexer_rejects_alphanumeric_and_underscore_strings_starting_with_number
  ) where

import Hedgehog
import Test.Hspec.Megaparsec
import Test.Tasty.Hspec

import Data.Char
import Minicute.Parser.Common ( Parser )
import Numeric
import Text.Megaparsec hiding ( failure )

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Minicute.Parser.Common.Lexer as L

spec_integer_lexer :: Spec
spec_integer_lexer = do
  describe "when an input has trailing spaces" $ do
    it "parses a integer" $ do
      (L.integer, "10  ") `shouldJustParseInto` 10
      (L.integer, "0b11\n") `shouldJustParseInto` 0b11
      (L.integer, "0o1252\t") `shouldJustParseInto` 0o1252
      (L.integer, "0d521 \t\n") `shouldJustParseInto` 521
      (L.integer, "0xfcab242\t\t  \t\t") `shouldJustParseInto` 0xfcab242

  describe "when an input has trailing alphabets" $ do
    it "fails to parse and leaves from the problematic character" $ do
      (L.integer, "0k") `failsLeavingInput` "k"
      (L.integer, "50a") `failsLeavingInput` "a"
      (L.integer, "204bjl") `failsLeavingInput` "bjl"
      (L.integer, "0b1101pqw") `failsLeavingInput` "pqw"
      (L.integer, "0X202PG") `failsLeavingInput` "PG"

hprop_integer_lexer_accepts_non_negative_integer_of_different_bases :: Property
hprop_integer_lexer_accepts_non_negative_integer_of_different_bases = do
  property $ do
    let nonNegativeIntegerRange = Range.constant 0 1000000000000
    i <- forAll $ Gen.integral nonNegativeIntegerRange
    iStr <- forAll $ genPrefixedIntegerString i
    i' <- evalEither $ parse (L.integer :: Parser Integer) "" iStr
    i === i'

hprop_integer_lexer_rejects_negative_integer :: Property
hprop_integer_lexer_rejects_negative_integer = do
  property $ do
    let negativeIntegerRange = Range.constant (negate 1000000000000) (negate 1)
    i <- forAll $ Gen.integral negativeIntegerRange
    iStr <- forAll $ genPrefixedIntegerString i
    case runParser' (L.integer :: Parser Integer) (initialState iStr) of
      (state, Left _) -> iStr === stateInput state
      (_, Right i') -> do
        annotateShow i'
        failure

genPrefixedIntegerString :: (MonadGen m) => Integer -> m String
genPrefixedIntegerString i = do
  (base, prefixes) <- Gen.element bases_and_prefixes
  prefix <- Gen.element prefixes
  pure (showBaseWithPrefix prefix base i)
  where
    bases_and_prefixes
      = [ (2, ["0b", "0B"])
        , (8, ["0o", "0O"])
        , (10, ["", "0d", "0D"])
        , (16, ["0x", "0X"])
        ]
    showBaseWithPrefix p b n
      = showSigned ((showString p .) . showIntAtBase b intToDigit) 0 n ""

hprop_integer_lexer_rejects_strings_having_number_prefix_starting_with_zero :: Property
hprop_integer_lexer_rejects_strings_having_number_prefix_starting_with_zero = do
  property $ do
    str <-
      forAll
      $ (<>)
      <$> Gen.string (Range.linear 1 100) Gen.digit
      <*> Gen.string (Range.linear 0 100) Gen.unicode
    case runParser' (L.integer :: Parser Integer) (initialState $ "0" <> str) of
      (state, Left _) -> str === stateInput state
      (_, Right n) -> do
        annotateShow n
        failure

hprop_integer_lexer_rejects_strings_without_integer_prefix :: Property
hprop_integer_lexer_rejects_strings_without_integer_prefix = do
  property $ do
    str <-
      forAll
      $ Gen.filter (not . hasIntegerPrefix)
      $ Gen.string (Range.linear 0 100) Gen.unicode
    case runParser' (L.integer :: Parser Integer) (initialState str) of
      (state, Left _) -> str === stateInput state
      (_, Right n) -> do
        annotateShow n
        failure
  where
    hasIntegerPrefix str = any ((str /=) . snd) . (reads :: ReadS Integer) $ str

hspec_identifier_lexer_accepts_alphabet_strings :: Property
hspec_identifier_lexer_accepts_alphabet_strings = do
  property $ do
    str <- forAll $ Gen.string (Range.linear 1 100) Gen.alpha
    ident <- evalEither $ parse (L.identifier :: Parser String) "" str
    str === ident

hspec_identifier_lexer_accepts_alphanumeric_and_underscore_strings_starting_with_alphabet :: Property
hspec_identifier_lexer_accepts_alphanumeric_and_underscore_strings_starting_with_alphabet = do
  property $ do
    str <-
      forAll
      $ (:)
      <$> Gen.alpha
      <*> Gen.string (Range.linear 1 100) (Gen.choice [Gen.alphaNum, Gen.constant '_'])
    ident <- evalEither $ parse (L.identifier :: Parser String) "" str
    str === ident

hspec_identifier_lexer_accepts_alphanumeric_and_underscore_strings_starting_with_underscore :: Property
hspec_identifier_lexer_accepts_alphanumeric_and_underscore_strings_starting_with_underscore = do
  property $ do
    str <-
      forAll
      $ ('_' :)
      <$> Gen.string (Range.linear 1 100) (Gen.choice [Gen.alphaNum, Gen.constant '_'])
    ident <- evalEither $ parse (L.identifier :: Parser String) "" str
    str === ident

hspec_identifier_lexer_rejects_alphanumeric_and_underscore_strings_starting_with_number :: Property
hspec_identifier_lexer_rejects_alphanumeric_and_underscore_strings_starting_with_number = do
  property $ do
    str <-
      forAll
      $ (:)
      <$> Gen.digit
      <*> Gen.string (Range.linear 1 100) Gen.alphaNum
    case runParser' (L.identifier :: Parser String) (initialState str) of
      (state, Left _) -> str === stateInput state
      (_, Right ident) -> do
        annotateShow ident
        failure


shouldParseInto :: (Show a, Eq a) => (Parser a, String) -> a -> String -> IO ()
shouldParseInto (p, content) value leaving = do
  parse p "" content `shouldParse` value
  runParser' p (initialState content) `succeedsLeaving` leaving

shouldJustParseInto :: (Show a, Eq a) => (Parser a, String) -> a -> IO ()
shouldJustParseInto pair value = shouldParseInto pair value ""

failsLeavingInput :: (Show a, Eq a) => (Parser a, String) -> String -> IO ()
failsLeavingInput (p, content) leaving = do
  runParser' p (initialState content) `failsLeaving` leaving
