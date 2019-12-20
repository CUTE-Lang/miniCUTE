{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
-- |
-- Copyright: (c) 2018-present Junyoung Clare Jang
-- License: BSD 3-Clause
--
-- Parser functions for miniCUTE
module Minicute.Parser.Minicute.Parser
  ( Parser

  , ExpressionType( .. )
  , ExpressionLevel( .. )
  , MainProgram
  , mainProgramMC
  , mainProgramLLMC
  ) where

import Prelude hiding ( fail )

import Control.Monad.Fail
import Control.Monad.Reader ( ReaderT, ask, mapReaderT, runReaderT )
import Data.Functor
import Data.List.Extra
import Minicute.Data.Minicute.Program
import Minicute.Parser.Common
import Minicute.Parser.Common.Parser
import Text.Megaparsec

import qualified Control.Monad.Combinators as Comb
import qualified Control.Monad.Combinators.Expr as CombExpr
import qualified Minicute.Parser.Common.Lexer as L

type WithPrecedence m = ReaderT (PrecedenceTable Primitive) m


-- |
-- A parser for 'MainProgramMC'
mainProgramMC :: Parser (MainProgram 'Simple 'MC)
mainProgramMC = program identifier (expressionMC identifier)
{-# INLINABLE mainProgramMC #-}

expressionMC :: (MonadParser e s m) => WithPrecedence m a -> WithPrecedence m (Expression 'Simple 'MC a)
expressionMC pA = go
  where
    go
      = choice
        [ letExpression pA go Recursive
        , letExpression pA go NonRecursive
        , matchExpression pA go
        , lambdaExpression pA go
        , otherExpressionsByPrec go
        ]
        <?> "expression"
{-# INLINABLE expressionMC #-}


-- |
-- A parser for 'MainProgramLLMC'
mainProgramLLMC :: Parser (MainProgram 'Simple 'LLMC)
mainProgramLLMC = program identifier (expressionLLMC identifier)
{-# INLINABLE mainProgramLLMC #-}

expressionLLMC :: (MonadParser e s m) => WithPrecedence m a -> WithPrecedence m (Expression 'Simple 'LLMC a)
expressionLLMC pA = go
  where
    go
      = choice
        [ letExpression pA go Recursive
        , letExpression pA go NonRecursive
        , matchExpression pA go
        , otherExpressionsByPrec go
        ]
        <?> "expression"
{-# INLINABLE expressionLLMC #-}


program :: (MonadParser e s m) => WithPrecedence m a -> WithPrecedence m (Expression t l a) -> m (Program t l a)
program pA pExpr = do
  void L.spacesConsumer
  result <-
    Program
    <$> runReaderT (supercombinators pA pExpr) primitivePrecedenceTable
  eof $> result
{-# INLINABLE program #-}


supercombinators :: (MonadParser e s m) => WithPrecedence m a -> WithPrecedence m (Expression t l a) -> WithPrecedence m [Supercombinator t l a]
supercombinators pA pExpr = sepEndBy (supercombinator pA pExpr) separator
{-# INLINABLE supercombinators #-}

supercombinator :: (MonadParser e s m) => WithPrecedence m a -> WithPrecedence m (Expression t l a) -> WithPrecedence m (Supercombinator t l a)
supercombinator pA pExpr
  = Supercombinator
    <$> ( (,,)
          <$> identifier
          <*> many pA <* L.symbol "="
          <*> pExpr
        )
{-# INLINABLE supercombinator #-}


lambdaExpression :: (MonadParser e s m) => WithPrecedence m a -> WithPrecedence m (Expression 'Simple 'MC a) -> WithPrecedence m (Expression 'Simple 'MC a)
lambdaExpression pA pExpr
  = ELambda ()
    <$> Comb.between (L.symbol "\\") (L.symbol "->") (some pA)
    <*> pExpr
    <?> "lambda expression"
{-# INLINABLE lambdaExpression #-}


letExpression :: (MonadParser e s m) => WithPrecedence m a -> WithPrecedence m (Expression 'Simple l a) -> IsRecursive -> WithPrecedence m (Expression 'Simple l a)
letExpression pA pExpr flag
  = ELet () flag
    <$> Comb.between (try startingKeyword) endingKeyword letDefinitions
    <*> pExpr
    <?> nameOfExpression
  where
    letDefinitions
      = (notFollowedBy endingKeyword <|> zeroLetDefinitionError)
        *> sepEndBy1 (letDefinition pA pExpr) separator

    startingKeyword
      | isRecursive flag = L.keyword "letrec"
      | otherwise = L.keyword "let"
    endingKeyword = L.keyword "in"

    zeroLetDefinitionError = fail (nameOfExpression <> " should include at least one definition")

    nameOfExpression
      | isRecursive flag = "letrec expression"
      | otherwise = "let expression"

    {-# INLINABLE letDefinitions #-}
    {-# INLINABLE startingKeyword #-}
    {-# INLINABLE endingKeyword #-}
    {-# INLINABLE zeroLetDefinitionError #-}
    {-# INLINABLE nameOfExpression #-}
{-# INLINABLE letExpression #-}

letDefinition :: (MonadParser e s m) => WithPrecedence m a -> WithPrecedence m (Expression t l a) -> WithPrecedence m (LetDefinition t l a)
letDefinition pA pExpr
  = LetDefinition
    <$> ( (,)
          <$> pA <* L.symbol "="
          <*> pExpr
        )
    <?> "let definition"
{-# INLINABLE letDefinition #-}

matchExpression :: (MonadParser e s m) => WithPrecedence m a -> WithPrecedence m (Expression 'Simple l a) -> WithPrecedence m (Expression 'Simple l a)
matchExpression pA pExpr
  = EMatch ()
    <$> Comb.between (try startingKeyword) endingKeyword pExpr
    <*> matchCases
    <?> "match expression"
  where
    matchCases
      = (notFollowedBy (separator <|> eof) <|> zeroMatchCaseError)
        *>
        ( (:)
          <$> matchCase pA pExpr
          <*> many (try (separator *> matchCase pA pExpr))
        )

    startingKeyword = L.keyword "match"
    endingKeyword = L.keyword "with"

    zeroMatchCaseError = fail "match expression should include at least one case"

    {-# INLINABLE matchCases #-}
    {-# INLINABLE startingKeyword #-}
    {-# INLINABLE endingKeyword #-}
    {-# INLINABLE zeroMatchCaseError #-}
{-# INLINABLE matchExpression #-}

matchCase :: (MonadParser e s m) => WithPrecedence m a -> WithPrecedence m (Expression t l a) -> WithPrecedence m (MatchCase t l a)
matchCase pA pExpr
  = MatchCase
    <$> ( (,,)
          <$> Comb.between (L.symbol "<") (L.symbol ">") L.integer
          <*> many pA <* L.symbol "->"
          <*> pExpr
        )
    <?> "match case"
{-# INLINABLE matchCase #-}


otherExpressionsByPrec
  :: (MonadParser e s m)
  => WithPrecedence m (Expression 'Simple l a)
  -> WithPrecedence m (Expression 'Simple l a)
otherExpressionsByPrec pExpr
  = ask
    >>=
    ( CombExpr.makeExprParser (applicationExpression pExpr)
      . createPrimitiveOperatorTable (EPrimitive ()) (EApplication ()) (EApplication2 () ())
    )
{-# INLINABLE otherExpressionsByPrec #-}

applicationExpression
  :: (MonadParser e s m)
  => WithPrecedence m (Expression 'Simple l a)
  -> WithPrecedence m (Expression 'Simple l a)
applicationExpression pExpr
  = foldl' (EApplication ()) <$> atomic <*> many atomic
  where
    atomic = atomicExpression pExpr

    {-# INLINABLE atomic #-}
{-# INLINABLE applicationExpression #-}

atomicExpression
  :: (MonadParser e s m)
  => WithPrecedence m (Expression 'Simple l a)
  -> WithPrecedence m (Expression 'Simple l a)
atomicExpression pExpr
  = choice
    [ EInteger () <$> L.integer <?> "integer"
    , EVariable () <$> identifier <?> "variable"
    , constructorExpression
    , mapReaderT L.betweenRoundBrackets pExpr <?> "expression with parentheses"
    ]
{-# INLINABLE atomicExpression #-}

-- |
-- The @startingSymbols@ do not use 'try' because
-- the keyword starts with a character that is illegal for identifiers.
constructorExpression :: (MonadParser e s m) => m (Expression 'Simple l a)
constructorExpression
  = Comb.between startingSymbols endingSymbols
    ( EConstructor ()
      <$> L.integer <* separator
      <*> L.integer
    )
    <?> "constructor"
  where
    startingSymbols = L.keyword "$C" *> L.symbol "{"
    endingSymbols = L.symbol "}"

    {-# INLINABLE startingSymbols #-}
    {-# INLINABLE endingSymbols #-}
{-# INLINABLE constructorExpression #-}

identifier :: (MonadParser e s m) => m Identifier
identifier = Identifier <$> L.identifier
{-# INLINABLE identifier #-}

separator :: (MonadParser e s m) => m ()
separator = L.symbol ";"
{-# INLINABLE separator #-}
