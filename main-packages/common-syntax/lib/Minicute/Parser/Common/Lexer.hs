{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Copyright: (c) 2018-present Junyoung Clare Jang
-- License: BSD 3-Clause
--
-- Common lexer functions used in miniCUTE compiler
module Minicute.Parser.Common.Lexer
  ( betweenRoundBrackets

  , gMachineIdentifier

  , identifier
  , keyword
  , symbol
  , integer

  , spacesConsumer
  ) where

import Prelude hiding ( fail )

import Control.Monad ( void )
import Control.Monad.Fail
import Data.Foldable
import Data.List.Extra
import Data.Monoid ( Any(..) )
import Data.Proxy
import Minicute.Parser.Common
import Text.Megaparsec hiding ( State )

import qualified Data.Char as Char
import qualified Text.Megaparsec.Char as MPT
import qualified Text.Megaparsec.Char.Lexer as MPTL

-- |
-- @betweenRoundBrackets p@ parses @"("@, and then @p@, and finally @")"@.
betweenRoundBrackets :: (MonadParser e s m) => m a -> m a
betweenRoundBrackets = between (symbol "(") (symbol ")")
{-# INLINABLE betweenRoundBrackets #-}


-- |
-- @gMachineIdentifier@ parses any character sequences not containing @';'@ or
-- space characters as an identifier.
gMachineIdentifier :: (MonadParser e s m) => m (Tokens s)
gMachineIdentifier
  = lexeme . many
    $ satisfy (getAny . foldMap (Any .) [(/= ';'), Char.isSpace])
{-# INLINABLE gMachineIdentifier #-}


-- I need to check whether identifier is a keyword or not
-- since I don't want to introduce additional separator for let definitions.
-- |
-- @identifier@ parses any character sequences starting with @identifierFirstChar@ followed by @identifierRestChar@.
-- For example, this function parses @"_abc"@, @"ab4"@, and @"a_ef_3"@ successfully.
--
-- For more examples, see the test module for this function.
identifier :: (MonadParser e s m) => m (Tokens s)
identifier = try identifier' <?> "identifier"
  where
    identifier' = do
      pos <- getOffset
      i <- lexeme (cons <$> identifierFirstChar <*> many identifierRestChar)
      checkKeywords pos i

    checkKeywords pos i
      | i `elem` keywordList = do
          setOffset pos
          fail $ "keyword " <> show i <> " cannot be an identifier"
      | otherwise = pure i

    {-# INLINABLE identifier' #-}
    {-# INLINABLE checkKeywords #-}
{-# INLINABLE identifier #-}

identifierFirstChar :: (MonadParser e s m) => m (Token s)
identifierFirstChar = MPT.letterChar <|> single '_'
{-# INLINABLE identifierFirstChar #-}

identifierRestChar :: (MonadParser e s m) => m (Token s)
identifierRestChar = MPT.alphaNumChar <|> single '_'
{-# INLINABLE identifierRestChar #-}

-- |
-- @keyword@ parses any keywords.
--
-- The keywords are @let@, @letrec@, @in@, @match@, @with@, and @$C@.
keyword :: (MonadParser e s m) => Tokens s -> m (Tokens s)
keyword k
  | k `elem` keywordList = lexeme (chunk k <* notFollowedBy identifierRestChar)
  | otherwise = error (k <> " is not a keyword")
{-# INLINABLE keyword #-}

keywordList :: [String]
keywordList
  = [ "let"
    , "letrec"
    , "in"
    , "match"
    , "with"
    , "$C"
    ]

-- |
-- @symbol str@ parses @str@ and ignore the result.
symbol :: (MonadParser e s m) => Tokens s -> m ()
symbol = void . MPTL.symbol spacesConsumer
{-# INLINABLE symbol #-}

-- |
-- @integer@ parses a decimal integer with no prefix,
-- binary integer with @"0b"@ or @"0B"@ prefix,
-- octal integer with @"0o"@ or @"0O"@ prefix,
-- decimal integer with @"0d"@ or @"0D"@ prefix,
-- hexadecimal integer with @"0x"@ or @"0X"@ prefix.
integer :: (MonadParser e s m, Integral a) => m a
integer
  = lexeme
    ( (integerStartWithZero <|> decimal)
      <* endOfInteger
    )
    <?> "integer"
  where
    integerStartWithZero
      = single '0'
        *> (prefixedInteger <|> zero)

    prefixedInteger = do
      b <- Char.toLower <$> satisfy ((`elem` ['b', 'o', 'd', 'x']) . Char.toLower)
      case b of
        'b' -> binary
        'o' -> octal
        'd' -> decimal
        'x' -> hexadecimal
        _ -> error ("Minicute.Parser.Lexer.integer: unexpected prefix " <> show b)

    zero
      = pure 0
        <?> "one of the integer prefixes ('b', 'B', 'o', 'O', 'd', 'D', 'x', 'X')"

    endOfInteger
      = notFollowedBy MPT.alphaNumChar
        <?> "non-alphanumeric"

    {-# INLINABLE integerStartWithZero #-}
    {-# INLINABLE zero #-}
    {-# INLINABLE endOfInteger #-}

lexeme :: (MonadParser e s m) => m a -> m a
lexeme = MPTL.lexeme spacesConsumer
{-# INLINABLE lexeme #-}

-- |
-- @spacesConsumer@ consumes all consecutive spaces and ignore the result.
spacesConsumer :: (MonadParser e s m) => m ()
spacesConsumer = hidden MPT.space
{-# INLINABLE spacesConsumer #-}

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

    {-# INLINE isBinDigit #-}
{-# INLINABLE binary #-}

octal :: forall e s m a. (MonadParser e s m, Integral a) => m a
octal
  = mkNumWithRadix 8 (Proxy :: Proxy s)
    <$> takeWhile1P (Just "octal digit") Char.isOctDigit
    <?> "octal integer"
{-# INLINABLE octal #-}

hexadecimal :: forall e s m a. (MonadParser e s m, Integral a) => m a
hexadecimal
  = mkNumWithRadix 16 (Proxy :: Proxy s)
    <$> takeWhile1P (Just "hexadecimal digit") Char.isHexDigit
    <?> "hexadecimal integer"
{-# INLINABLE hexadecimal #-}

mkNumWithRadix :: forall proxy s a. (Stream s, Token s ~ Char, Integral a) => a -> proxy s -> Tokens s -> a
mkNumWithRadix radix _ = foldl' step 0 . chunkToTokens (Proxy :: Proxy s)
  where
    step a = (a * radix +) . fromIntegral . Char.digitToInt

    {-# INLINABLE step #-}
{-# INLINABLE mkNumWithRadix #-}
