{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Minicute.Parser.Lexer
  ( betweenRoundBrackets
  , identifier
  , keyword
  , symbol
  , integer
  , spacesConsumer
  ) where

import Control.Monad ( void )
import Data.List
import Data.List.Extra
import Data.Proxy
import Minicute.Parser.Types
import Text.Megaparsec hiding ( State )

import qualified Data.Char as Char
import qualified Text.Megaparsec.Char as MPT
import qualified Text.Megaparsec.Char.Lexer as MPTL

betweenRoundBrackets :: (MonadParser e s m) => m a -> m a
betweenRoundBrackets = between (symbol "(") (symbol ")")
{-# INLINEABLE betweenRoundBrackets #-}

-- |
-- I need to check whether identifier is a keyword or not
-- since I don't want to introduce additional separator for @match ... with@
identifier :: (MonadParser e s m) => m s
identifier = try identifier' <?> "identifier"
  where
    identifier' = do
      pos <- getOffset
      i <- lexeme (cons <$> identifierFirstChar <*> many identifierRestChar)
      checkKeywords pos i
    {-# INLINEABLE identifier' #-}

    checkKeywords pos i
      | i `elem` keywordList = do
          setOffset pos
          fail $ "keyword " <> show i <> " cannot be an identifier"
      | otherwise = return i
    {-# INLINEABLE checkKeywords #-}

identifierFirstChar :: (MonadParser e s m) => m (Token s)
identifierFirstChar = MPT.letterChar <|> MPT.char '_'
{-# INLINEABLE identifierFirstChar #-}

identifierRestChar :: (MonadParser e s m) => m (Token s)
identifierRestChar = MPT.alphaNumChar <|> MPT.char '_'
{-# INLINEABLE identifierRestChar #-}

keyword :: (MonadParser e s m) => Tokens s -> m (Tokens s)
keyword k
  | k `elem` keywordList = lexeme (chunk k <* notFollowedBy identifierRestChar)
  | otherwise = error (k <> " is not a keyword")

keywordList :: [String]
keywordList
  = [ "let"
    , "letrec"
    , "in"
    , "match"
    , "with"
    , "$C"
    ]

symbol :: forall e s m. (MonadParser e s m) => Tokens s -> m ()
symbol = void . MPTL.symbol spacesConsumer
{-# INLINEABLE symbol #-}

integer :: (MonadParser e s m, Integral a) => m a
integer
  = lexeme
    ( (integerStartWithZero <|> decimal)
      <* endOfInteger
    )
    <?> "integer"
  where
    integerStartWithZero
      = MPT.char '0'
        *> (prefixedInteger <|> zero)
    {-# INLINEABLE integerStartWithZero #-}

    prefixedInteger = do
      b <- Char.toLower <$> oneOf ['b', 'B', 'o', 'O', 'd', 'D', 'x', 'X']
      case b of
        'b' -> binary
        'o' -> octal
        'd' -> decimal
        'x' -> hexadecimal
        _ -> error ("Minicute.Parser.Lexer.integer: unexpected prefix " <> show b)

    zero
      = return 0
        <?> "one of the integer prefixes ('b', 'B', 'o', 'O', 'd', 'D', 'x', 'X')"

    endOfInteger
      = notFollowedBy MPT.alphaNumChar
        <?> "non-alphanumeric"
    {-# INLINEABLE zero #-}

lexeme :: (MonadParser e s m) => m a -> m a
lexeme = MPTL.lexeme spacesConsumer
{-# INLINEABLE lexeme #-}

spacesConsumer :: (MonadParser e s m) => m ()
spacesConsumer = hidden MPT.space
{-# INLINEABLE spacesConsumer #-}

decimal :: forall e s m a. (MonadParser e s m, Integral a) => m a
decimal
  = mkNumWithRadix 10 (Proxy :: Proxy s)
    <$> takeWhile1P (Just "decimal digit") Char.isDigit
    <?> "decimal integer"
{-# INLINE decimal #-}

binary :: forall e s m a. (MonadParser e s m, Integral a) => m a
binary
  = mkNumWithRadix 2 (Proxy :: Proxy s)
    <$> takeWhile1P (Just "binary digit") isBinDigit
    <?> "binary integer"
  where
    isBinDigit x = x == '0' || x == '1'
{-# INLINEABLE binary #-}

octal :: forall e s m a. (MonadParser e s m, Integral a) => m a
octal
  = mkNumWithRadix 8 (Proxy :: Proxy s)
    <$> takeWhile1P (Just "octal digit") Char.isOctDigit
    <?> "octal integer"
{-# INLINEABLE octal #-}

hexadecimal :: forall e s m a. (MonadParser e s m, Integral a) => m a
hexadecimal
  = mkNumWithRadix 16 (Proxy :: Proxy s)
    <$> takeWhile1P (Just "hexadecimal digit") Char.isHexDigit
    <?> "hexadecimal integer"
{-# INLINEABLE hexadecimal #-}

mkNumWithRadix :: (Stream s, Token s ~ Char, Integral a) => a -> Proxy s -> Tokens s -> a
mkNumWithRadix radix = (foldl' step 0 .) . chunkToTokens
  where
    step a = (a * radix +) . fromIntegral . Char.digitToInt
{-# INLINEABLE mkNumWithRadix #-}
