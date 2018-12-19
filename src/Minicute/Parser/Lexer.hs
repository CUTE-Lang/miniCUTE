{-# LANGUAGE TypeFamilies #-}
module Minicute.Parser.Lexer where

import Control.Arrow
import Control.Monad ( void )
import Data.List
import Data.Void
import Text.Megaparsec

import qualified Data.Char as Char
import qualified Text.Megaparsec.Char as MPC
import qualified Text.Megaparsec.Char.Lexer as MPCL

type Parser = Parsec Void String

integer :: Parser Integer
integer = prefixedInteger <|> decimal
  where
    prefixedInteger =
      do
        void $ MPC.char '0'
        b <- Char.toLower <$> MPC.oneOf "bBoOdDxX"
        case b of
          'b' -> binary
          'o' -> octal
          'd' -> decimal
          'x' -> hexadecimal
          _ -> fail $ ""

binary :: Parser Integer
binary = lexeme (binary_ <?> "binary integer")
  where
    binary_ :: Parser Integer
    binary_ = mkNum <$> takeWhile1P Nothing isBinary

    mkNum = foldl' step 0

    step acc ch = acc * 2 + fromIntegral (Char.digitToInt ch)

    isBinary c = c == '0' || c == '1'
{-# INLINEABLE binary #-}

decimal :: Parser Integer
decimal = lexeme MPCL.decimal
{-# INLINEABLE decimal #-}

octal :: Parser Integer
octal = lexeme MPCL.octal
{-# INLINEABLE octal #-}

hexadecimal :: Parser Integer
hexadecimal = lexeme MPCL.hexadecimal
{-# INLINEABLE hexadecimal #-}

lexeme :: Parser a -> Parser a
lexeme = MPCL.lexeme spacesConsumer
{-# INLINEABLE lexeme #-}

spacesConsumer :: Parser ()
spacesConsumer = hidden MPC.space
{-# INLINEABLE spacesConsumer #-}
