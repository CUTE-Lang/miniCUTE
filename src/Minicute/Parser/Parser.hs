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

import qualified Control.Monad.Combinators as Comb
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

precedenceTable :: (MonadParser e s m) => m PrecedenceTable
precedenceTable = return defaultPrecedenceTable

supercombinatorL :: (MonadParser e s m) => WithPrecedence m MainSupercombinatorL
supercombinatorL
  = (,,)
    <$> L.identifier
    <*> many L.identifier <* L.symbol "="
    <*> expressionL
    <?> "top-level definition"

expressionL :: (MonadParser e s m) => WithPrecedence m MainExpressionL
expressionL
  = letExpressionL Recursive
    <|> letExpressionL NonRecursive
    <|> matchExpressionL
    <|> lambdaExpressionL
    <|> otherExpressionsByPrec
    <?> "expression"

letExpressionL :: (MonadParser e s m) => IsRecursive -> WithPrecedence m MainExpressionL
letExpressionL flag
  = try
    ( ELLet flag
      <$> Comb.between startingKeyword endingKeyword letDefinitionsL
      <*> expressionL
    )
    <?> nameOfExpression
  where
    startingKeyword
      | isRecursive flag = L.symbol "letrec"
      | otherwise = L.symbol "let"
    endingKeyword = L.symbol "in"

    letDefinitionsL = sepEndBy1 letDefinitionL separator

    nameOfExpression
      | isRecursive flag = "letrec expression"
      | otherwise = "let expression"

letDefinitionL :: (MonadParser e s m) => WithPrecedence m MainLetDefinitionL
letDefinitionL
  = (,)
    <$> L.identifier <* L.symbol "="
    <*> expressionL
    <?> "let definition"

matchExpressionL :: (MonadParser e s m) => WithPrecedence m MainExpressionL
matchExpressionL
  = ELMatch
    <$> Comb.between startingKeyword endingKeyword expressionL
    <*> sepBy1 matchCaseL separator
    <?> "match expression"
  where
    startingKeyword = L.symbol "match"
    endingKeyword = L.symbol "with"

matchCaseL :: (MonadParser e s m) => WithPrecedence m MainMatchCaseL
matchCaseL
  = (,,)
    <$> Comb.between (L.symbol "<") (L.symbol ">") L.integer
    <*> many L.identifier <* L.symbol "->"
    <*> expressionL
    <?> "match case"

lambdaExpressionL :: (MonadParser e s m) => WithPrecedence m MainExpressionL
lambdaExpressionL
  = L.symbol "\\"
    *>
    ( ELLambda
      <$> many L.identifier <* L.symbol "->"
      <*> expressionL
    )
    <?> "lambda expression"

otherExpressionsByPrec :: (MonadParser e s m) => WithPrecedence m MainExpressionL
otherExpressionsByPrec = ask >>= CombExpr.makeExprParser applicationExpressionL . precedenceTableToOperatorTable

applicationExpressionL :: (MonadParser e s m) => WithPrecedence m MainExpressionL
applicationExpressionL
  = makeApplicationChain <$> CombNE.some atomicExpressionL
  where
    makeApplicationChain (aExpr :| aExprs) = foldl' ELApplication aExpr aExprs

atomicExpressionL :: (MonadParser e s m) => WithPrecedence m MainExpressionL
atomicExpressionL
  = integerExpression
    <|> constructorExpression
    <|> variableExpression
    <|> mapReaderT L.betweenRoundBrackets expressionL

integerExpression :: (MonadParser e s m) => m MainExpressionL
integerExpression = ELInteger <$> L.integer <?> "integer expression"

variableExpression :: (MonadParser e s m) => m MainExpressionL
variableExpression = ELVariable <$> L.identifier <?> "variable identifier"

constructorExpression :: (MonadParser e s m) => m MainExpressionL
constructorExpression
  = L.symbol "Pack"
    *> Comb.between (L.symbol "{") (L.symbol "}")
    ( ELConstructor
      <$> L.integer <* L.symbol ","
      <*> L.integer
    )
    <?> "constructor expression"

separator :: (MonadParser e s m) => m ()
separator = L.symbol ";"

precedenceTableToOperatorTable :: (MonadParser e s m) => PrecedenceTable -> OperatorTable m
precedenceTableToOperatorTable = fmap (fmap precedenceTableEntryToOperator) . groupSortOn (negate . precedence . snd)

precedenceTableEntryToOperator :: (MonadParser e s m) => PrecedenceTableEntry -> Operator m
precedenceTableEntryToOperator (op, PInfixN _) = CombExpr.InfixN (L.symbol op $> ELApplication2 (ELVariable op))
precedenceTableEntryToOperator (op, PInfixL _) = CombExpr.InfixL (L.symbol op $> ELApplication2 (ELVariable op))
precedenceTableEntryToOperator (op, PInfixR _) = CombExpr.InfixR (L.symbol op $> ELApplication2 (ELVariable op))
precedenceTableEntryToOperator (op, PPrefix _) = CombExpr.Prefix (L.symbol op $> ELApplication (ELVariable op))
precedenceTableEntryToOperator (op, PPostfix _) = CombExpr.Postfix (L.symbol op $> ELApplication (ELVariable op))
