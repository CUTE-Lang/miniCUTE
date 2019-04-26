{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Minicute.Parser.Parser
  ( Parser

  , MainProgramL
  , programL
  ) where

import Control.Monad.Reader ( runReaderT, mapReaderT, ask )
import Data.List.Extra
import Data.Functor
import Minicute.Parser.Types
import Minicute.Types.Minicute.Program
import Text.Megaparsec

import qualified Control.Monad.Combinators as Comb
import qualified Control.Monad.Combinators.Expr as CombExpr
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
{-# INLINEABLE precedenceTable #-}

supercombinatorL :: (MonadParser e s m, m ~ Parser) => WithPrecedence m MainSupercombinatorL
supercombinatorL
  = (,,)
    <$> L.identifier
    <*> many L.identifier <* L.symbol "="
    <*> expressionL
{-# INLINEABLE supercombinatorL #-}

expressionL :: (MonadParser e s m, m ~ Parser) => WithPrecedence m MainExpressionL
expressionL
  = choice
    [ letExpressionL Recursive
    , letExpressionL NonRecursive
    , matchExpressionL
    , lambdaExpressionL
    , otherExpressionsByPrec
    ]
    <?> "expression"
{-# INLINEABLE expressionL #-}

letExpressionL :: (MonadParser e s m, m ~ Parser) => IsRecursive -> WithPrecedence m MainExpressionL
letExpressionL flag
  = ELLet flag
    <$> Comb.between (try startingKeyword) endingKeyword letDefinitionsL
    <*> expressionL
    <?> nameOfExpression
  where
    letDefinitionsL
      = (notFollowedBy endingKeyword <|> zeroLetDefinitionError)
        *> sepEndBy1 letDefinitionL separator

    startingKeyword
      | isRecursive flag = L.keyword "letrec"
      | otherwise = L.keyword "let"
    endingKeyword = L.keyword "in"

    zeroLetDefinitionError = fail (nameOfExpression <> " should include at least one definition")

    nameOfExpression
      | isRecursive flag = "letrec expression"
      | otherwise = "let expression"

    {-# INLINEABLE letDefinitionsL #-}
    {-# INLINEABLE startingKeyword #-}
    {-# INLINEABLE endingKeyword #-}
    {-# INLINEABLE zeroLetDefinitionError #-}
    {-# INLINEABLE nameOfExpression #-}

letDefinitionL :: (MonadParser e s m, m ~ Parser) => WithPrecedence m MainLetDefinitionL
letDefinitionL
  = (,)
    <$> L.identifier <* L.symbol "="
    <*> expressionL
    <?> "let definition"
{-# INLINEABLE letDefinitionL #-}

matchExpressionL :: (MonadParser e s m, m ~ Parser) => WithPrecedence m MainExpressionL
matchExpressionL
  = ELMatch
    <$> Comb.between (try startingKeyword) endingKeyword expressionL
    <*> matchCasesL
    <?> "match expression"
  where
    matchCasesL
      = (notFollowedBy (separator <|> eof) <|> zeroMatchCaseError)
        *>
        ( cons
          <$> matchCaseL
          <*> many (try (separator *> matchCaseL))
        )

    startingKeyword = L.keyword "match"
    endingKeyword = L.keyword "with"

    zeroMatchCaseError = fail "match expression should include at least one case"

    {-# INLINEABLE matchCasesL #-}
    {-# INLINEABLE startingKeyword #-}
    {-# INLINEABLE endingKeyword #-}
    {-# INLINEABLE zeroMatchCaseError #-}

matchCaseL :: (MonadParser e s m, m ~ Parser) => WithPrecedence m MainMatchCaseL
matchCaseL
  = (,,)
    <$> Comb.between (L.symbol "<") (L.symbol ">") L.integer
    <*> many L.identifier <* L.symbol "->"
    <*> expressionL
    <?> "match case"
{-# INLINEABLE matchCaseL #-}

lambdaExpressionL :: (MonadParser e s m, m ~ Parser) => WithPrecedence m MainExpressionL
lambdaExpressionL
  = ELLambda
    <$> Comb.between (L.symbol "\\") (L.symbol "->") (some L.identifier)
    <*> expressionL
    <?> "lambda expression"
{-# INLINEABLE lambdaExpressionL #-}

otherExpressionsByPrec :: (MonadParser e s m, m ~ Parser) => WithPrecedence m MainExpressionL
otherExpressionsByPrec = ask >>= CombExpr.makeExprParser applicationExpressionL . createOperatorTable
{-# INLINEABLE otherExpressionsByPrec #-}

applicationExpressionL :: (MonadParser e s m, m ~ Parser) => WithPrecedence m MainExpressionL
applicationExpressionL
  = foldl' ELApplication <$> atomicExpressionL <*> many atomicExpressionL
{-# INLINEABLE applicationExpressionL #-}

atomicExpressionL :: (MonadParser e s m, m ~ Parser) => WithPrecedence m MainExpressionL
atomicExpressionL
  = choice
    [ integerExpressionL
    , variableExpressionL
    , constructorExpressionL
    , mapReaderT L.betweenRoundBrackets expressionL <?> "expression with parentheses"
    ]
{-# INLINEABLE atomicExpressionL #-}

integerExpressionL :: (MonadParser e s m) => m MainExpressionL
integerExpressionL = ELInteger <$> L.integer <?> "integer"
{-# INLINEABLE integerExpressionL #-}

variableExpressionL :: (MonadParser e s m) => m MainExpressionL
variableExpressionL = ELVariable <$> L.identifier <?> "variable"
{-# INLINEABLE variableExpressionL #-}

-- |
-- The 'startingSymbols' do not use `try` because
-- the keyword starts with a character that is illegal for identifiers.
constructorExpressionL :: (MonadParser e s m) => m MainExpressionL
constructorExpressionL
  = Comb.between startingSymbols endingSymbols
    ( ELConstructor
      <$> L.integer <* separator
      <*> L.integer
    )
    <?> "constructor"
  where
    startingSymbols = L.keyword "$C" *> L.symbol "{"
    endingSymbols = L.symbol "}"

    {-# INLINEABLE startingSymbols #-}
    {-# INLINEABLE endingSymbols #-}
{-# INLINEABLE constructorExpressionL #-}

separator :: (MonadParser e s m) => m ()
separator = L.symbol ";"
{-# INLINEABLE separator #-}

createOperatorTable :: (MonadParser e s m) => PrecedenceTable -> [[CombExpr.Operator m MainExpressionL]]
createOperatorTable = (fmap . fmap) createOperator . groupSortOn (negate . precedence . snd)
{-# INLINEABLE createOperatorTable #-}

createOperator :: (MonadParser e s m) => PrecedenceTableEntry -> CombExpr.Operator m MainExpressionL
createOperator (op, PInfixN _) = CombExpr.InfixN ((L.symbol op <?> "binary operator") $> ELApplication2 (ELVariable op))
createOperator (op, PInfixL _) = CombExpr.InfixL ((L.symbol op <?> "binary operator") $> ELApplication2 (ELVariable op))
createOperator (op, PInfixR _) = CombExpr.InfixR ((L.symbol op <?> "binary operator") $> ELApplication2 (ELVariable op))
createOperator (op, PPrefix _) = CombExpr.Prefix ((L.symbol op <?> "binary operator") $> ELApplication (ELVariable op))
createOperator (op, PPostfix _) = CombExpr.Postfix ((L.symbol op <?> "binary operator") $> ELApplication (ELVariable op))
