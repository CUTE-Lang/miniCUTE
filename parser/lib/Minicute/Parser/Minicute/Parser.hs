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

import Control.Monad.Reader ( ReaderT, ask, mapReaderT, runReaderT )
import Data.Functor
import Data.List.Extra
import Minicute.Data.Minicute.Precedence
import Minicute.Data.Minicute.Program
import Minicute.Parser.Common
import Text.Megaparsec

import qualified Control.Monad.Combinators as Comb
import qualified Control.Monad.Combinators.Expr as CombExpr
import qualified Minicute.Parser.Lexer as L

type WithPrecedence m = ReaderT PrecedenceTable m


-- |
-- A parser for 'MainProgramMC'
mainProgramMC :: Parser MainProgramMC
mainProgramMC = program identifier (expressionMC identifier)
{-# INLINEABLE mainProgramMC #-}

expressionMC :: (MonadParser e s m) => WithPrecedence m a -> WithPrecedence m (ExpressionMC a)
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
{-# INLINEABLE expressionMC #-}


-- |
-- A parser for 'MainProgramLLMC'
mainProgramLLMC :: Parser MainProgramLLMC
mainProgramLLMC = program identifier (expressionLLMC identifier)
{-# INLINEABLE mainProgramLLMC #-}

expressionLLMC :: (MonadParser e s m) => WithPrecedence m a -> WithPrecedence m (ExpressionLLMC a)
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
{-# INLINEABLE expressionLLMC #-}


program :: (MonadParser e s m) => WithPrecedence m a -> WithPrecedence m (expr a) -> m (Program expr a)
program pA pExpr = do
  void L.spacesConsumer
  ps <- getParserState
  pt <- precedenceTable
  setParserState ps
  result <- Program <$> runReaderT (sepEndBy (supercombinator pA pExpr) (L.symbol ";")) pt
  void eof
  return result


precedenceTable :: (MonadParser e s m) => m PrecedenceTable
precedenceTable = return defaultPrecedenceTable
{-# INLINEABLE precedenceTable #-}


supercombinator :: (MonadParser e s m) => WithPrecedence m a -> WithPrecedence m (expr a) -> WithPrecedence m (Supercombinator expr a)
supercombinator pA pExpr
  = Supercombinator
    <$> ( (,,)
          <$> identifier
          <*> many pA <* L.symbol "="
          <*> pExpr
        )
{-# INLINEABLE supercombinator #-}


lambdaExpression :: (MonadParser e s m) => WithPrecedence m a -> WithPrecedence m (ExpressionMC a) -> WithPrecedence m (ExpressionMC a)
lambdaExpression pA pExpr
  = ELambda
    <$> Comb.between (L.symbol "\\") (L.symbol "->") (some pA)
    <*> pExpr
    <?> "lambda expression"
{-# INLINEABLE lambdaExpression #-}


letExpression :: (MonadParser e s m) => WithPrecedence m a -> WithPrecedence m (Expression t a) -> IsRecursive -> WithPrecedence m (Expression t a)
letExpression pA pExpr flag
  = ELet flag
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

matchExpression :: (MonadParser e s m) => WithPrecedence m a -> WithPrecedence m (Expression t a) -> WithPrecedence m (Expression t a)
matchExpression pA pExpr
  = EMatch
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
  => WithPrecedence m (Expression t a)
  -> WithPrecedence m (Expression t a)
otherExpressionsByPrec pExpr
  = ask
    >>= CombExpr.makeExprParser (applicationExpression pExpr)
    . createOperatorTable EVariable EApplication EApplication2
{-# INLINEABLE otherExpressionsByPrec #-}

applicationExpression
  :: (MonadParser e s m)
  => WithPrecedence m (Expression t a)
  -> WithPrecedence m (Expression t a)
applicationExpression pExpr
  = foldl' EApplication <$> atomic <*> many atomic
  where
    atomic = atomicExpression pExpr
{-# INLINEABLE applicationExpression #-}

atomicExpression
  :: (MonadParser e s m)
  => WithPrecedence m (Expression t a)
  -> WithPrecedence m (Expression t a)
atomicExpression pExpr
  = choice
    [ EInteger <$> L.integer <?> "integer"
    , EVariable <$> identifier <?> "variable"
    , constructorExpression
    , mapReaderT L.betweenRoundBrackets pExpr <?> "expression with parentheses"
    ]
{-# INLINEABLE atomicExpression #-}

-- |
-- The @startingSymbols@ do not use 'try' because
-- the keyword starts with a character that is illegal for identifiers.
constructorExpression :: (MonadParser e s m) => m (Expression t a)
constructorExpression
  = Comb.between startingSymbols endingSymbols
    ( EConstructor
      <$> L.integer <* separator
      <*> L.integer
    )
    <?> "constructor"
  where
    startingSymbols = L.keyword "$C" *> L.symbol "{"
    endingSymbols = L.symbol "}"

    {-# INLINEABLE startingSymbols #-}
    {-# INLINEABLE endingSymbols #-}
{-# INLINEABLE constructorExpression #-}

identifier :: (MonadParser e s m) => m Identifier
identifier = Identifier <$> L.identifier

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
