{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Minicute.Parser.Parser
  ( Parser

  , MainProgramL
  , programL
  ) where

import Control.Monad.Reader ( runReaderT, mapReaderT, ask )
import Data.List.Extra
import Data.List.NonEmpty ( NonEmpty( (:|) ) )
import Data.Functor
import Minicute.Parser.Types
import Minicute.Types.Minicute.Program
import Text.Megaparsec

import qualified Control.Monad.Combinators.Expr as CombExpr
import qualified Control.Monad.Combinators.NonEmpty as CombNE
import qualified Minicute.Parser.Lexer as L

programL :: Parser MainProgramL
programL = do
  void L.spacesConsumer
  ps <- getParserState
  pt <- precedenceTable
  setParserState ps
  program <- ProgramL <$> runReaderT (sepEndBy supercombinatorL (L.symbol ";")) pt
  void (hidden eof)
  return program

precedenceTable :: (MonadParser e s m, m ~ Parser) => m PrecedenceTable
precedenceTable = return defaultPrecedenceTable

supercombinatorL :: (MonadParser e s m, m ~ Parser) => WithPrecedence m MainSupercombinatorL
supercombinatorL = (,,) <$> L.identifier <*> (many L.identifier <* L.symbol "=") <*> expressionL

expressionL :: (MonadParser e s m, m ~ Parser) => WithPrecedence m MainExpressionL
expressionL
  = letExpressionL Recursive
    <|> letExpressionL NonRecursive
    <|> otherExpressionsByPrec
    <?> "expression"

letExpressionL :: (MonadParser e s m, m ~ Parser) => IsRecursive -> WithPrecedence m MainExpressionL
letExpressionL flag
  = try (L.symbol keyword *> (ELLet flag <$> sepEndBy1 (try letDefinitionL) (L.symbol ";") <* L.symbol "in" <*> expressionL))
    <?> parserName
  where
    keyword
      | isRecursive flag = "letrec"
      | otherwise = "let"
    parserName
      | isRecursive flag = "letrec expression"
      | otherwise = "let expression"

letDefinitionL :: (MonadParser e s m, m ~ Parser) => WithPrecedence m MainLetDefinitionL
letDefinitionL = (,) <$> L.identifier <* L.symbol "=" <*> expressionL

otherExpressionsByPrec :: (MonadParser e s m, m ~ Parser) => WithPrecedence m MainExpressionL
otherExpressionsByPrec = ask >>= CombExpr.makeExprParser applicationExpressionL . precedenceTableToOperatorTable

applicationExpressionL :: (MonadParser e s m, m ~ Parser) => WithPrecedence m MainExpressionL
applicationExpressionL
  = makeApplicationChain <$> CombNE.some atomicExpressionL
  where
    makeApplicationChain (aExpr :| aExprs) = foldl' ELApplication aExpr aExprs

atomicExpressionL :: (MonadParser e s m, m ~ Parser) => WithPrecedence m MainExpressionL
atomicExpressionL
  = integerExpression
    <|> constructorExpression
    <|> variableExpression
    <|> mapReaderT L.betweenRoundBrackets expressionL

integerExpression :: (MonadParser e s m) => m MainExpressionL
integerExpression = ELInteger <$> L.integer

-- |
-- Should I check whether identifier is a keyword or not?
variableExpression :: (MonadParser e s m) => m MainExpressionL
variableExpression = ELVariable <$> L.identifier

constructorExpression :: (MonadParser e s m) => m MainExpressionL
constructorExpression = L.symbol "Pack" *> L.symbol "{" *> (ELConstructor <$> L.integer <* L.symbol "," <*> L.integer) <* L.symbol "}"

precedenceTableToOperatorTable :: (MonadParser e s m) => PrecedenceTable -> OperatorTable m
precedenceTableToOperatorTable = fmap (fmap precedenceTableEntryToOperator) . groupSortOn (negate . precedence . snd)

precedenceTableEntryToOperator :: (MonadParser e s m) => PrecedenceTableEntry -> Operator m
precedenceTableEntryToOperator (op, PInfixN _) = CombExpr.InfixN (L.symbol op $> ELApplication2 (ELVariable op))
precedenceTableEntryToOperator (op, PInfixL _) = CombExpr.InfixL (L.symbol op $> ELApplication2 (ELVariable op))
precedenceTableEntryToOperator (op, PInfixR _) = CombExpr.InfixR (L.symbol op $> ELApplication2 (ELVariable op))
precedenceTableEntryToOperator (op, PPrefix _) = CombExpr.Prefix (L.symbol op $> ELApplication (ELVariable op))
precedenceTableEntryToOperator (op, PPostfix _) = CombExpr.Postfix (L.symbol op $> ELApplication (ELVariable op))
