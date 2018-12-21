module Minicute.Parser.TestUtils
  ( runParserTest
  ) where

import Data.Void
import Text.Megaparsec

import qualified Minicute.Parser.Lexer as L

runParserTest :: L.Parser a -> String -> Either (ParseError (Token String) Void) a
runParserTest p = parse p ""
