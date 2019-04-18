{-# LANGUAGE TypeFamilies #-}
module Minicute.Parser.Lexer
  ( betweenRoundBrackets
  , identifier
  , symbol
  , integer
  , spacesConsumer
  ) where

import Control.Monad ( void )
import Minicute.Parser.Types
import Text.Megaparsec hiding ( State )

import qualified Data.Char as Char
import qualified Text.Megaparsec.Char as MPC
import qualified Text.Megaparsec.Char.Lexer as MPCL

betweenRoundBrackets :: (MonadParser e s m) => m a -> m a
betweenRoundBrackets = between (symbol "(") (symbol ")")
{-# INLINEABLE betweenRoundBrackets #-}

-- |
-- I need to check whether identifier is a keyword or not
-- since I don't want to introduce additional separator for @match ... with@
identifier :: (MonadParser e s m) => m String
identifier = try (identifier' >>= checkKeywords) <?> "identifier"
  where
    identifier' = lexeme ((:) <$> identifierFirstChar <*> many identifierRestChar)
    {-# INLINEABLE identifier' #-}

    checkKeywords i
      | i `elem` keywords = fail $ "keyword " <> show i <> " cannot be an identifier"
      | otherwise = return i
    {-# INLINEABLE checkKeywords #-}

keywords :: [String]
keywords
  = [ "let"
    , "letrec"
    , "in"
    , "match"
    , "with"
    ]

identifierFirstChar :: (MonadParser e s m) => m Char
identifierFirstChar = MPC.letterChar <|> MPC.char '_' <?> "alphabet or _"
{-# INLINEABLE identifierFirstChar #-}

identifierRestChar :: (MonadParser e s m) => m Char
identifierRestChar = MPC.alphaNumChar <|> MPC.char '_' <?> "alphanumeric or _"
{-# INLINEABLE identifierRestChar #-}

symbol :: (MonadParser e s m) => String -> m ()
symbol = void . MPCL.symbol spacesConsumer
{-# INLINEABLE symbol #-}

integer :: (MonadParser e s m, Integral a) => m a
integer
  = lexeme
    ( try
      ( (integerStartWithZero <|> MPCL.decimal)
        <* notFollowedBy MPC.alphaNumChar
      )
    )
    <?> "integer"
  where
    integerStartWithZero = do
      void . MPC.char $ '0'
      prefixedInteger <|> zero
    {-# INLINEABLE integerStartWithZero #-}

    prefixedInteger = do
      b <- Char.toLower <$> oneOf "bBoOdDxX"
      case b of
        'b' -> MPCL.binary
        'o' -> MPCL.octal
        'd' -> MPCL.decimal
        'x' -> MPCL.hexadecimal
        _ -> error ("Minicute.Parser.Lexer.integer: unexpected prefix " <> show b)

    zero
      = return 0
        <?> "one of the integer prefixes ('b', 'B', 'o', 'O', 'd', 'D', 'x', 'X')"
    {-# INLINEABLE zero #-}

lexeme :: (MonadParser e s m) => m a -> m a
lexeme = MPCL.lexeme spacesConsumer
{-# INLINEABLE lexeme #-}

spacesConsumer :: (MonadParser e s m) => m ()
spacesConsumer = hidden MPC.space
{-# INLINEABLE spacesConsumer #-}
