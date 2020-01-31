{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Use camelCase" -}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Copyright: (c) 2018-present Junyoung Clare Jang
-- License: BSD 3-Clause
module Minicute.Test.Common.GenTest
  ( hprop_integer_gen_is_compatible_with_integer_lexer

  , hprop_identifierString_gen_is_compatible_with_identifier_lexer
  ) where

import Hedgehog

import Minicute.Parser.Common ( Parser )
import Text.Megaparsec hiding ( failure )

import qualified Minicute.Parser.Common.Lexer as L
import qualified Minicute.Test.Common.Gen as Gen

hprop_integer_gen_is_compatible_with_integer_lexer :: Property
hprop_integer_gen_is_compatible_with_integer_lexer = do
  property $ do
    i <- forAll Gen.integer
    i' <- evalEither $ parse (L.integer :: Parser Integer) "" (show i)
    i === i'

hprop_identifierString_gen_is_compatible_with_identifier_lexer :: Property
hprop_identifierString_gen_is_compatible_with_identifier_lexer = do
  property $ do
    ident <- forAll Gen.identifierString
    ident' <- evalEither $ parse (L.identifier :: Parser String) "" ident
    ident === ident'
