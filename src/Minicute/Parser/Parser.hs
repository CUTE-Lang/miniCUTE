module Minicute.Parser.Parser where

import Data.Void
import Minicute.Parser.Lexer

import qualified Minicute.Common.Program as P
import qualified Text.Megaparsec as MP

program :: Parser P.Program
program = P.Program <$> expression

expression :: Parser P.Expression
expression = integerExpression

integerExpression :: Parser P.Expression
integerExpression = P.IntegerExpression <$> integer
