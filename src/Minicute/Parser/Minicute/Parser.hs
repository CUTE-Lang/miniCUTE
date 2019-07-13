{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Parser functions for miniCUTE
module Minicute.Parser.Minicute.Parser
  ( Parser

  , MainProgramMC
  , mainProgramMC

  , MainProgramLLMC
  , mainProgramLLMC
  ) where

import Control.Monad.Reader ( ReaderT, runReaderT, mapReaderT, ask )
import Data.List.Extra
import Data.Functor
import Data.Tuple.Extra
import Minicute.Parser.Common
import Minicute.Data.Minicute.Precedence
import Minicute.Data.Minicute.Program
import Text.Megaparsec

import qualified Control.Monad.Combinators as Comb
import qualified Control.Monad.Combinators.Expr as CombExpr
import qualified Minicute.Parser.Lexer as L

type WithPrecedence m = ReaderT PrecedenceTable m


-- |
-- A parser for 'MainProgramMC'
mainProgramMC :: Parser MainProgramMC
mainProgramMC = program L.identifier (expressionMC L.identifier)
{-# INLINEABLE mainProgramMC #-}

expressionMC :: (MonadParser e s m) => WithPrecedence m a -> WithPrecedence m (ExpressionMC a)
expressionMC pA = go
  where
    go
      = choice
        [ uncurry3 ELet <$> letExpressionFields pA go Recursive
        , uncurry3 ELet <$> letExpressionFields pA go NonRecursive
        , uncurry EMatch <$> matchExpressionFields pA go
        , uncurry ELambda <$> lambdaExpressionFields pA go
        , otherExpressionsByPrec EInteger EVariable EConstructor EApplication go
        ]
        <?> "expression"
{-# INLINEABLE expressionMC #-}


-- |
-- A parser for 'MainProgramLLMC'
mainProgramLLMC :: Parser MainProgramLLMC
mainProgramLLMC = program L.identifier (expressionLLMC L.identifier)
{-# INLINEABLE mainProgramLLMC #-}

expressionLLMC :: (MonadParser e s m) => WithPrecedence m a -> WithPrecedence m (ExpressionLLMC a)
expressionLLMC pA = go
  where
    go
      = choice
        [ uncurry3 ELet <$> letExpressionFields pA go Recursive
        , uncurry3 ELet <$> letExpressionFields pA go NonRecursive
        , uncurry EMatch <$> matchExpressionFields pA go
        , otherExpressionsByPrec EInteger EVariable EConstructor EApplication go
        ]
        <?> "expression"
{-# INLINEABLE expressionLLMC #-}


program :: (MonadParser e s m) => WithPrecedence m a -> WithPrecedence m (expr a) -> m (Program expr a)
program pA pExpr = do
  void L.spacesConsumer
  ps <- getParserState
  pt <- precedenceTable
  setParserState ps
  result <- Program <$> runReaderT (sepEndBy (Supercombinator <$> supercombinatorFields pA pExpr) (L.symbol ";")) pt
  void eof
  return result


precedenceTable :: (MonadParser e s m) => m PrecedenceTable
precedenceTable = return defaultPrecedenceTable
{-# INLINEABLE precedenceTable #-}


supercombinatorFields :: (MonadParser e s m) => WithPrecedence m a -> WithPrecedence m (expr a) -> WithPrecedence m (Identifier, [a], expr a)
supercombinatorFields pA pExpr
  = (,,)
    <$> L.identifier
    <*> many pA <* L.symbol "="
    <*> pExpr
{-# INLINEABLE supercombinatorFields #-}


lambdaExpressionFields :: (MonadParser e s m) => WithPrecedence m a -> WithPrecedence m (expr a) -> WithPrecedence m ([a], expr a)
lambdaExpressionFields pA pExpr
  = (,)
    <$> Comb.between (L.symbol "\\") (L.symbol "->") (some pA)
    <*> pExpr
    <?> "lambda expression"
{-# INLINEABLE lambdaExpressionFields #-}


letExpressionFields :: (MonadParser e s m) => WithPrecedence m a -> WithPrecedence m (expr a) -> IsRecursive -> WithPrecedence m (IsRecursive, [LetDefinition expr a], expr a)
letExpressionFields pA pExpr flag
  = (,,) flag
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

    {-# INLINEABLE letDefinitions #-}
    {-# INLINEABLE startingKeyword #-}
    {-# INLINEABLE endingKeyword #-}
    {-# INLINEABLE zeroLetDefinitionError #-}
    {-# INLINEABLE nameOfExpression #-}

letDefinition :: (MonadParser e s m) => WithPrecedence m a -> WithPrecedence m (expr a) -> WithPrecedence m (LetDefinition expr a)
letDefinition pA pExpr
  = LetDefinition
    <$> ( (,)
          <$> pA <* L.symbol "="
          <*> pExpr
        )
    <?> "let definition"
{-# INLINEABLE letDefinition #-}

matchExpressionFields :: (MonadParser e s m) => WithPrecedence m a -> WithPrecedence m (expr a) -> WithPrecedence m (expr a, [MatchCase expr a])
matchExpressionFields pA pExpr
  = (,)
    <$> Comb.between (try startingKeyword) endingKeyword pExpr
    <*> matchCases
    <?> "match expression"
  where
    matchCases
      = (notFollowedBy (separator <|> eof) <|> zeroMatchCaseError)
        *>
        ( cons
          <$> matchCase pA pExpr
          <*> many (try (separator *> matchCase pA pExpr))
        )

    startingKeyword = L.keyword "match"
    endingKeyword = L.keyword "with"

    zeroMatchCaseError = fail "match expression should include at least one case"

    {-# INLINEABLE matchCases #-}
    {-# INLINEABLE startingKeyword #-}
    {-# INLINEABLE endingKeyword #-}
    {-# INLINEABLE zeroMatchCaseError #-}

matchCase :: (MonadParser e s m) => WithPrecedence m a -> WithPrecedence m (expr a) -> WithPrecedence m (MatchCase expr a)
matchCase pA pExpr
  = MatchCase
    <$> ( (,,)
          <$> Comb.between (L.symbol "<") (L.symbol ">") L.integer
          <*> many pA <* L.symbol "->"
          <*> pExpr
        )
    <?> "match case"
{-# INLINEABLE matchCase #-}


otherExpressionsByPrec
  :: (MonadParser e s m)
  => (Integer -> expr a)
  -> (Identifier -> expr a)
  -> (Integer -> Integer -> expr a)
  -> (expr a -> expr a -> expr a)
  -> WithPrecedence m (expr a)
  -> WithPrecedence m (expr a)
otherExpressionsByPrec int var con app pExpr
  = ask
    >>= CombExpr.makeExprParser (applicationExpression int var con app pExpr)
    . createOperatorTable var app ((app .) . app)
{-# INLINEABLE otherExpressionsByPrec #-}

applicationExpression
  :: (MonadParser e s m)
  => (Integer -> expr a)
  -> (Identifier -> expr a)
  -> (Integer -> Integer -> expr a)
  -> (expr a -> expr a -> expr a)
  -> WithPrecedence m (expr a)
  -> WithPrecedence m (expr a)
applicationExpression int var con app pExpr
  = foldl' app <$> atomic <*> many atomic
  where
    atomic = atomicExpression int var con pExpr
{-# INLINEABLE applicationExpression #-}

atomicExpression
  :: (MonadParser e s m)
  => (Integer -> expr a)
  -> (Identifier -> expr a)
  -> (Integer -> Integer -> expr a)
  -> WithPrecedence m (expr a)
  -> WithPrecedence m (expr a)
atomicExpression int var con pExpr
  = choice
    [ int <$> integerExpressionFields
    , var <$> variableExpressionFields
    , uncurry con <$> constructorExpressionFields
    , mapReaderT L.betweenRoundBrackets pExpr <?> "expression with parentheses"
    ]
{-# INLINEABLE atomicExpression #-}

integerExpressionFields :: (MonadParser e s m) => m Integer
integerExpressionFields = L.integer <?> "integer"
{-# INLINEABLE integerExpressionFields #-}

variableExpressionFields :: (MonadParser e s m) => m Identifier
variableExpressionFields = L.identifier <?> "variable"
{-# INLINEABLE variableExpressionFields #-}

-- |
-- The @startingSymbols@ do not use 'try' because
-- the keyword starts with a character that is illegal for identifiers.
constructorExpressionFields :: (MonadParser e s m) => m (Integer, Integer)
constructorExpressionFields
  = Comb.between startingSymbols endingSymbols
    ( (,)
      <$> L.integer <* separator
      <*> L.integer
    )
    <?> "constructor"
  where
    startingSymbols = L.keyword "$C" *> L.symbol "{"
    endingSymbols = L.symbol "}"

    {-# INLINEABLE startingSymbols #-}
    {-# INLINEABLE endingSymbols #-}
{-# INLINEABLE constructorExpressionFields #-}


separator :: (MonadParser e s m) => m ()
separator = L.symbol ";"
{-# INLINEABLE separator #-}


createOperatorTable :: (MonadParser e s m) => (Identifier -> expr) -> (expr -> expr -> expr) -> (expr -> expr -> expr -> expr) -> PrecedenceTable -> [[CombExpr.Operator m expr]]
createOperatorTable cVar cUn cBar = (fmap . fmap) (createOperator cVar cUn cBar) . groupSortOn (negate . precedence . snd)
{-# INLINEABLE createOperatorTable #-}

createOperator :: (MonadParser e s m) => (Identifier -> expr) -> (expr -> expr -> expr) -> (expr -> expr -> expr -> expr) -> PrecedenceTableEntry -> CombExpr.Operator m expr
createOperator cVar _ cBin (op, PInfixN _) = CombExpr.InfixN (createOperatorBinParser cVar cBin op)
createOperator cVar _ cBin (op, PInfixL _) = CombExpr.InfixL (createOperatorBinParser cVar cBin op)
createOperator cVar _ cBin (op, PInfixR _) = CombExpr.InfixR (createOperatorBinParser cVar cBin op)
createOperator cVar cUn _ (op, PPrefix _) = CombExpr.Prefix (createOperatorUnParser cVar cUn op)
createOperator cVar cUn _ (op, PPostfix _) = CombExpr.Postfix (createOperatorUnParser cVar cUn op)

createOperatorBinParser :: (MonadParser e s m) => (Identifier -> expr) -> (expr -> expr -> expr -> expr) -> Identifier -> m (expr -> expr -> expr)
createOperatorBinParser cVar cBin op@(Identifier opName) = (L.symbol opName <?> "binary operator") $> cBin (cVar op)
{-# INLINEABLE createOperatorBinParser #-}

createOperatorUnParser :: (MonadParser e s m) => (Identifier -> expr) -> (expr -> expr -> expr) -> Identifier -> m (expr -> expr)
createOperatorUnParser cVar cUn op@(Identifier opName) = (L.symbol opName <?> "unary operator") $> cUn (cVar op)
{-# INLINEABLE createOperatorUnParser #-}
