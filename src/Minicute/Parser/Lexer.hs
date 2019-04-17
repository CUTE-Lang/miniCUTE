{-# LANGUAGE TypeFamilies #-}
module Minicute.Parser.Lexer
  ( betweenRoundBrackets
  , identifier
  , symbol
  , string
  , integer
  , spacesConsumer
  ) where

import Control.Monad ( void )
import Data.Functor
import Text.Megaparsec hiding ( State )

import qualified Data.Char as Char
import qualified Text.Megaparsec.Char as MPC
import qualified Text.Megaparsec.Char.Lexer as MPCL

betweenRoundBrackets :: (MonadParsec e s m, s ~ String) => m a -> m a
betweenRoundBrackets = between (symbol "(") (symbol ")")
{-# INLINEABLE betweenRoundBrackets #-}

identifier :: (MonadParsec e s m, s ~ String) => m String
identifier = lexeme ((:) <$> identifierFirstChar <*> many identifierRestChar) <?> "identifier"

identifierFirstChar :: (MonadParsec e s m, s ~ String) => m Char
identifierFirstChar = MPC.letterChar <|> MPC.char '_' <?> "alphabet or _"
{-# INLINEABLE identifierFirstChar #-}

identifierRestChar :: (MonadParsec e s m, s ~ String) => m Char
identifierRestChar = MPC.alphaNumChar <|> MPC.char '_' <?> "alphanumeric or _"
{-# INLINEABLE identifierRestChar #-}

symbol :: (MonadParsec e s m, s ~ String) => String -> m ()
symbol = void . MPCL.symbol spacesConsumer
{-# INLINEABLE symbol #-}

string :: (MonadParsec e s m, s ~ String) => String -> m String
string = lexeme . MPC.string
{-# INLINEABLE string #-}

integer :: (MonadParsec e s m, s ~ String, Integral a) => m a
integer = lexeme (integerStartWithZero <|> MPCL.decimal) <?> "integer"
  where
    integerStartWithZero = do
      void . MPC.char $ '0'
      try prefixedInteger <|> zero

    prefixedInteger = do
      b <- Char.toLower <$> oneOf "bBoOdDxX"
      case b of
        'b' -> MPCL.binary
        'o' -> MPCL.octal
        'd' -> MPCL.decimal
        'x' -> MPCL.hexadecimal
        _ -> error ("Minicute.Parser.Lexer.integer: unexpected prefix " <> show b)

    zero
      = notFollowedBy MPC.alphaNumChar $> 0
        <?> "one of the integer prefixes ('b', 'B', 'o', 'O', 'd', 'D', 'x', 'X')"

lexeme :: (MonadParsec e s m, s ~ String) => m a -> m a
lexeme = MPCL.lexeme spacesConsumer
{-# INLINEABLE lexeme #-}

spacesConsumer :: (MonadParsec e s m, s ~ String) => m ()
spacesConsumer = hidden MPC.space
{-# INLINEABLE spacesConsumer #-}
