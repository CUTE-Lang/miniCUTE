{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Minicute.Parser.Parser
  ( Parser

  , MainProgramL
  , mainProgramL
  ) where

import Control.Monad.Reader ( runReaderT, mapReaderT, ask )
import Data.List.Extra
import Data.Functor
import Minicute.Data.Fix
import Minicute.Parser.Types
import Minicute.Types.Minicute.Program
import Text.Megaparsec

import qualified Control.Monad.Combinators as Comb
import qualified Control.Monad.Combinators.Expr as CombExpr
import qualified Minicute.Parser.Lexer as L

mainProgramL :: Parser MainProgramL
mainProgramL = programL L.identifier

programL :: (MonadParser e s m) => WithPrecedence m a -> m (ProgramL a)
programL pA = program_ pA (expressionL pA)

expressionL :: (MonadParser e s m) => WithPrecedence m a -> WithPrecedence m (ExpressionL a)
expressionL pA
  = choice
    [ ELExpression <$> letExpression_ pA (expressionL pA) Recursive
    , ELExpression <$> letExpression_ pA (expressionL pA) NonRecursive
    , ELExpression <$> matchExpression_ pA (expressionL pA)
    , Fix2 <$> lambdaExpressionL_ pA (expressionL pA)
    , otherExpressionsByPrecL pA
    ]
    <?> "expression"
{-# INLINEABLE expressionL #-}

otherExpressionsByPrecL :: (MonadParser e s m) => WithPrecedence m a -> WithPrecedence m (ExpressionL a)
otherExpressionsByPrecL pA
  = ask >>= CombExpr.makeExprParser (applicationExpressionL pA) . createOperatorTableL
{-# INLINEABLE otherExpressionsByPrecL #-}

applicationExpressionL :: (MonadParser e s m) => WithPrecedence m a -> WithPrecedence m (ExpressionL a)
applicationExpressionL pA
  = foldl' ELApplication <$> atomicExpressionL pA <*> many (atomicExpressionL pA)
{-# INLINEABLE applicationExpressionL #-}

atomicExpressionL :: (MonadParser e s m) => WithPrecedence m a -> WithPrecedence m (ExpressionL a)
atomicExpressionL pA
  = choice
    [ ELExpression <$> integerExpression_
    , ELExpression <$> variableExpression_
    , ELExpression <$> constructorExpression_
    , mapReaderT L.betweenRoundBrackets (expressionL pA) <?> "expression with parentheses"
    ]
{-# INLINEABLE atomicExpressionL #-}


program_ :: (MonadParser e s m) => WithPrecedence m a -> WithPrecedence m (expr a) -> m (Program_ expr a)
program_ pA pExpr = do
  void L.spacesConsumer
  ps <- getParserState
  pt <- precedenceTable
  setParserState ps
  program <- Program_ <$> runReaderT (sepEndBy (supercombinator_ pA pExpr) (L.symbol ";")) pt
  void eof
  return program

precedenceTable :: (MonadParser e s m) => m PrecedenceTable
precedenceTable = return defaultPrecedenceTable
{-# INLINEABLE precedenceTable #-}


supercombinator_ :: (MonadParser e s m) => WithPrecedence m a -> WithPrecedence m (expr a) -> WithPrecedence m (Supercombinator_ expr a)
supercombinator_ pA pExpr
  = (,,)
    <$> L.identifier
    <*> many pA <* L.symbol "="
    <*> pExpr
{-# INLINEABLE supercombinator_ #-}


lambdaExpressionL_ :: (MonadParser e s m) => WithPrecedence m a -> WithPrecedence m (expr_ a) -> WithPrecedence m (ExpressionL_ expr_ a)
lambdaExpressionL_ pA pExpr
  = ELLambda_
    <$> Comb.between (L.symbol "\\") (L.symbol "->") (some pA)
    <*> pExpr
    <?> "lambda expression"
{-# INLINEABLE lambdaExpressionL_ #-}

letExpression_ :: (MonadParser e s m) => WithPrecedence m a -> WithPrecedence m (expr_ a) -> IsRecursive -> WithPrecedence m (Expression_ expr_ a)
letExpression_ pA pExpr flag
  = ELet_ flag
    <$> Comb.between (try startingKeyword) endingKeyword letDefinitions_
    <*> pExpr
    <?> nameOfExpression
  where
    letDefinitions_
      = (notFollowedBy endingKeyword <|> zeroLetDefinitionError)
        *> sepEndBy1 (letDefinition_ pA pExpr) separator

    startingKeyword
      | isRecursive flag = L.keyword "letrec"
      | otherwise = L.keyword "let"
    endingKeyword = L.keyword "in"

    zeroLetDefinitionError = fail (nameOfExpression <> " should include at least one definition")

    nameOfExpression
      | isRecursive flag = "letrec expression"
      | otherwise = "let expression"

    {-# INLINEABLE letDefinitions_ #-}
    {-# INLINEABLE startingKeyword #-}
    {-# INLINEABLE endingKeyword #-}
    {-# INLINEABLE zeroLetDefinitionError #-}
    {-# INLINEABLE nameOfExpression #-}

letDefinition_ :: (MonadParser e s m) => WithPrecedence m a -> WithPrecedence m (expr_ a) -> WithPrecedence m (LetDefinition_ expr_ a)
letDefinition_ pA pExpr
  = (,)
    <$> pA <* L.symbol "="
    <*> pExpr
    <?> "let definition"
{-# INLINEABLE letDefinition_ #-}

matchExpression_ :: (MonadParser e s m) => WithPrecedence m a -> WithPrecedence m (expr_ a) -> WithPrecedence m (Expression_ expr_ a)
matchExpression_ pA pExpr
  = EMatch_
    <$> Comb.between (try startingKeyword) endingKeyword pExpr
    <*> matchCases_
    <?> "match expression"
  where
    matchCases_
      = (notFollowedBy (separator <|> eof) <|> zeroMatchCaseError)
        *>
        ( cons
          <$> matchCase_ pA pExpr
          <*> many (try (separator *> matchCase_ pA pExpr))
        )

    startingKeyword = L.keyword "match"
    endingKeyword = L.keyword "with"

    zeroMatchCaseError = fail "match expression should include at least one case"

    {-# INLINEABLE matchCases_ #-}
    {-# INLINEABLE startingKeyword #-}
    {-# INLINEABLE endingKeyword #-}
    {-# INLINEABLE zeroMatchCaseError #-}

matchCase_ :: (MonadParser e s m) => WithPrecedence m a -> WithPrecedence m (expr_ a) -> WithPrecedence m (MatchCase_ expr_ a)
matchCase_ pA pExpr
  = (,,)
    <$> Comb.between (L.symbol "<") (L.symbol ">") L.integer
    <*> many pA <* L.symbol "->"
    <*> pExpr
    <?> "match case"
{-# INLINEABLE matchCase_ #-}

integerExpression_ :: (MonadParser e s m) => m (Expression_ expr_ a)
integerExpression_ = EInteger_ <$> L.integer <?> "integer"
{-# INLINEABLE integerExpression_ #-}

variableExpression_ :: (MonadParser e s m) => m (Expression_ expr_ a)
variableExpression_ = EVariable_ <$> L.identifier <?> "variable"
{-# INLINEABLE variableExpression_ #-}

-- |
-- The 'startingSymbols' do not use `try` because
-- the keyword starts with a character that is illegal for identifiers.
constructorExpression_ :: (MonadParser e s m) => m (Expression_ expr_ a)
constructorExpression_
  = Comb.between startingSymbols endingSymbols
    ( EConstructor_
      <$> L.integer <* separator
      <*> L.integer
    )
    <?> "constructor"
  where
    startingSymbols = L.keyword "$C" *> L.symbol "{"
    endingSymbols = L.symbol "}"

    {-# INLINEABLE startingSymbols #-}
    {-# INLINEABLE endingSymbols #-}
{-# INLINEABLE constructorExpression_ #-}

separator :: (MonadParser e s m) => m ()
separator = L.symbol ";"
{-# INLINEABLE separator #-}

createOperatorTableL :: (MonadParser e s m) => PrecedenceTable -> [[CombExpr.Operator m (ExpressionL a)]]
createOperatorTableL = (fmap . fmap) createOperatorL . groupSortOn (negate . precedence . snd)
{-# INLINEABLE createOperatorTableL #-}

createOperatorL :: (MonadParser e s m) => PrecedenceTableEntry -> CombExpr.Operator m (ExpressionL a)
createOperatorL (op, PInfixN _) = CombExpr.InfixN ((L.symbol op <?> "binary operator") $> ELApplication2 (ELVariable op))
createOperatorL (op, PInfixL _) = CombExpr.InfixL ((L.symbol op <?> "binary operator") $> ELApplication2 (ELVariable op))
createOperatorL (op, PInfixR _) = CombExpr.InfixR ((L.symbol op <?> "binary operator") $> ELApplication2 (ELVariable op))
createOperatorL (op, PPrefix _) = CombExpr.Prefix ((L.symbol op <?> "binary operator") $> ELApplication (ELVariable op))
createOperatorL (op, PPostfix _) = CombExpr.Postfix ((L.symbol op <?> "binary operator") $> ELApplication (ELVariable op))
