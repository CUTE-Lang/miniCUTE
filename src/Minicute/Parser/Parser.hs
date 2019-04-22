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
  void eof
  return program

precedenceTable :: (MonadParser e s m, m ~ Parser) => m PrecedenceTable
precedenceTable = return defaultPrecedenceTable

supercombinatorL :: (MonadParser e s m, m ~ Parser) => WithPrecedence m MainSupercombinatorL
supercombinatorL
  = (,,)
    <$> L.identifier
    <*> many L.identifier <* L.symbol "="
    <*> expressionL

expressionL :: (MonadParser e s m, m ~ Parser) => WithPrecedence m MainExpressionL
expressionL
  = letExpressionL Recursive
    <|> letExpressionL NonRecursive
    <|> matchExpressionL
    <|> lambdaExpressionL
    <|> otherExpressionsByPrec
    <?> "expression"

letExpressionL :: (MonadParser e s m, m ~ Parser) => IsRecursive -> WithPrecedence m MainExpressionL
letExpressionL flag
  = ELLet flag
    <$> Comb.between startingKeyword endingKeyword letDefinitionsL
    <*> expressionL
    <?> nameOfExpression
  where
    letDefinitionsL
      = (notFollowedBy endingKeyword <|> zeroLetDefinitionError)
        *> sepEndBy1 letDefinitionL separator

    startingKeyword
      | isRecursive flag = try (L.keyword "letrec")
      | otherwise = try (L.keyword "let")
    endingKeyword = L.keyword "in"

    zeroLetDefinitionError = fail (nameOfExpression <> " should include at least one definition")

    nameOfExpression
      | isRecursive flag = "letrec expression"
      | otherwise = "let expression"

letDefinitionL :: (MonadParser e s m, m ~ Parser) => WithPrecedence m MainLetDefinitionL
letDefinitionL
  = (,)
    <$> L.identifier <* L.symbol "="
    <*> expressionL
    <?> "let definition"

matchExpressionL :: (MonadParser e s m, m ~ Parser) => WithPrecedence m MainExpressionL
matchExpressionL
  = ELMatch
    <$> Comb.between startingKeyword endingKeyword expressionL
    <*> matchCasesL
    <?> "match expression"
  where
    matchCasesL
      = (notFollowedBy (separator <|> eof) <|> zeroMatchCaseError)
        *> sepBy1 matchCaseL separator

    startingKeyword = try (L.keyword "match")
    endingKeyword = L.keyword "with"

    zeroMatchCaseError = fail "match expression should include at least one case"

matchCaseL :: (MonadParser e s m, m ~ Parser) => WithPrecedence m MainMatchCaseL
matchCaseL
  = (,,)
    <$> Comb.between (L.symbol "<") (L.symbol ">") L.integer
    <*> many L.identifier <* L.symbol "->"
    <*> expressionL
    <?> "match case"

lambdaExpressionL :: (MonadParser e s m, m ~ Parser) => WithPrecedence m MainExpressionL
lambdaExpressionL
  = ELLambda
    <$> Comb.between (L.symbol "\\") (L.symbol "->") (many L.identifier)
    <*> expressionL
    <?> "lambda expression"

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
    <|> variableExpression
    <|> constructorExpression
    <|> (mapReaderT L.betweenRoundBrackets expressionL <?> "expression with parentheses")

integerExpression :: (MonadParser e s m) => m MainExpressionL
integerExpression = ELInteger <$> L.integer <?> "integer"

variableExpression :: (MonadParser e s m) => m MainExpressionL
variableExpression = ELVariable <$> L.identifier <?> "variable"

constructorExpression :: (MonadParser e s m) => m MainExpressionL
constructorExpression
  = Comb.between startingSymbols endingSymbols
    ( ELConstructor
      <$> L.integer <* separator
      <*> L.integer
    )
    <?> "constructor"
  where
    startingSymbols = L.keyword "$C" *> L.symbol "{"
    endingSymbols = L.symbol "}"

separator :: (MonadParser e s m) => m ()
separator = L.symbol ";"

precedenceTableToOperatorTable :: (MonadParser e s m) => PrecedenceTable -> OperatorTable m
precedenceTableToOperatorTable = fmap (fmap precedenceTableEntryToOperator) . groupSortOn (negate . precedence . snd)

precedenceTableEntryToOperator :: (MonadParser e s m) => PrecedenceTableEntry -> Operator m
precedenceTableEntryToOperator (op, PInfixN _) = CombExpr.InfixN ((L.symbol op <?> "binary operator") $> ELApplication2 (ELVariable op))
precedenceTableEntryToOperator (op, PInfixL _) = CombExpr.InfixL ((L.symbol op <?> "binary operator") $> ELApplication2 (ELVariable op))
precedenceTableEntryToOperator (op, PInfixR _) = CombExpr.InfixR ((L.symbol op <?> "binary operator") $> ELApplication2 (ELVariable op))
precedenceTableEntryToOperator (op, PPrefix _) = CombExpr.Prefix ((L.symbol op <?> "binary operator") $> ELApplication (ELVariable op))
precedenceTableEntryToOperator (op, PPostfix _) = CombExpr.Postfix ((L.symbol op <?> "binary operator") $> ELApplication (ELVariable op))
