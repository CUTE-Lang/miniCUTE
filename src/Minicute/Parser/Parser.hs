{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Minicute.Parser.Parser
  ( Parser

  , MainProgramL
  , mainProgramL

  , MainProgram
  , mainProgram
  ) where

import Control.Monad.Reader ( ReaderT, runReaderT, mapReaderT, ask )
import Data.List.Extra
import Data.Functor
import Minicute.Data.Fix
import Minicute.Parser.Types
import Minicute.Types.Minicute.Precedence
import Minicute.Types.Minicute.Program
import Text.Megaparsec

import qualified Control.Monad.Combinators as Comb
import qualified Control.Monad.Combinators.Expr as CombExpr
import qualified Minicute.Parser.Lexer as L

type WithPrecedence m = ReaderT PrecedenceTable m


mainProgramL :: Parser MainProgramL
mainProgramL = programL identifier
{-# INLINEABLE mainProgramL #-}

programL :: (MonadParser e s m) => WithPrecedence m a -> m (ProgramL a)
programL pA = program_ pA (expressionL pA)
{-# INLINEABLE programL #-}


expressionL :: (MonadParser e s m) => WithPrecedence m a -> WithPrecedence m (ExpressionL a)
expressionL pA = go
  where
    go
      = choice
        [ ELExpression <$> complexExpression_ pA go
        , Fix2 <$> lambdaExpressionL_ pA go
        , otherExpressionsByPrec_ ELExpression pA go
        ]
        <?> "expression"
{-# INLINEABLE expressionL #-}


mainProgram :: Parser MainProgram
mainProgram = program identifier
{-# INLINEABLE mainProgram #-}

program :: (MonadParser e s m) => WithPrecedence m a -> m (Program a)
program pA = program_ pA (expression pA)
{-# INLINEABLE program #-}


expression :: (MonadParser e s m) => WithPrecedence m a -> WithPrecedence m (Expression a)
expression pA = go
  where
    go
      = choice
        [ Fix2 <$> complexExpression_ pA go
        , otherExpressionsByPrec_ Fix2 pA go
        ]
        <?> "expression"
{-# INLINEABLE expression #-}


program_ :: (MonadParser e s m) => WithPrecedence m a -> WithPrecedence m (expr a) -> m (Program_ expr a)
program_ pA pExpr = do
  void L.spacesConsumer
  ps <- getParserState
  pt <- precedenceTable
  setParserState ps
  result <- Program_ <$> runReaderT (sepEndBy (supercombinator_ pA pExpr) (L.symbol ";")) pt
  void eof
  return result

precedenceTable :: (MonadParser e s m) => m PrecedenceTable
precedenceTable = return defaultPrecedenceTable
{-# INLINEABLE precedenceTable #-}


supercombinator_ :: (MonadParser e s m) => WithPrecedence m a -> WithPrecedence m (expr a) -> WithPrecedence m (Supercombinator_ expr a)
supercombinator_ pA pExpr
  = Supercombinator_
    <$> ( (,,)
          <$> identifier
          <*> many pA <* L.symbol "="
          <*> pExpr
        )
{-# INLINEABLE supercombinator_ #-}


lambdaExpressionL_ :: (MonadParser e s m) => WithPrecedence m a -> WithPrecedence m (expr_ a) -> WithPrecedence m (ExpressionL_ expr_ a)
lambdaExpressionL_ pA pExpr
  = ELLambda_
    <$> Comb.between (L.symbol "\\") (L.symbol "->") (some pA)
    <*> pExpr
    <?> "lambda expression"
{-# INLINEABLE lambdaExpressionL_ #-}


complexExpression_ :: (MonadParser e s m) => WithPrecedence m a -> WithPrecedence m (expr_ a) -> WithPrecedence m (Expression_ expr_ a)
complexExpression_ pA pExpr
  = choice
    [ letExpression_ pA pExpr Recursive
    , letExpression_ pA pExpr NonRecursive
    , matchExpression_ pA pExpr
    ]

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
  = LetDefinition_
    <$> ( (,)
          <$> pA <* L.symbol "="
          <*> pExpr
        )
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
  = MatchCase_
    <$> ( (,,)
          <$> Comb.between (L.symbol "<") (L.symbol ">") L.integer
          <*> many pA <* L.symbol "->"
          <*> pExpr
        )
    <?> "match case"
{-# INLINEABLE matchCase_ #-}


otherExpressionsByPrec_
  :: (MonadParser e s m)
  => (Expression_ expr_ a -> expr_ a)
  -> WithPrecedence m a
  -> WithPrecedence m (expr_ a)
  -> WithPrecedence m (expr_ a)
otherExpressionsByPrec_ wrap pA pExpr
  = ask
    >>= CombExpr.makeExprParser (applicationExpression_ wrap pA pExpr)
    . createOperatorTable_ eVariable_ eApplication_ eApplication2_
  where
    eVariable_ v = wrap (EVariable_ (Identifier v))
    eApplication_ e1 e2 = wrap (EApplication_ e1 e2)
    eApplication2_ e1 e2 e3 = wrap (EApplication_ (wrap (EApplication_ e1 e2)) e3)
{-# INLINEABLE otherExpressionsByPrec_ #-}

applicationExpression_
  :: (MonadParser e s m)
  => (Expression_ expr_ a -> expr_ a)
  -> WithPrecedence m a
  -> WithPrecedence m (expr_ a)
  -> WithPrecedence m (expr_ a)
applicationExpression_ wrap pA pExpr
  = foldl' ((wrap .) . EApplication_) <$> atomicExpression_ wrap pA pExpr <*> many (atomicExpression_ wrap pA pExpr)
{-# INLINEABLE applicationExpression_ #-}

atomicExpression_
  :: (MonadParser e s m)
  => (Expression_ expr_ a -> expr_ a)
  -> WithPrecedence m a
  -> WithPrecedence m (expr_ a)
  -> WithPrecedence m (expr_ a)
atomicExpression_ wrap _ pExpr
  = choice
    [ wrap <$> integerExpression_
    , wrap <$> variableExpression_
    , wrap <$> constructorExpression_
    , mapReaderT L.betweenRoundBrackets pExpr <?> "expression with parentheses"
    ]
{-# INLINEABLE atomicExpression_ #-}

integerExpression_ :: (MonadParser e s m) => m (Expression_ expr_ a)
integerExpression_ = EInteger_ <$> L.integer <?> "integer"
{-# INLINEABLE integerExpression_ #-}

variableExpression_ :: (MonadParser e s m) => m (Expression_ expr_ a)
variableExpression_ = EVariable_ <$> identifier <?> "variable"
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


identifier :: (MonadParser e s m) => m Identifier
identifier = Identifier <$> L.identifier

separator :: (MonadParser e s m) => m ()
separator = L.symbol ";"
{-# INLINEABLE separator #-}


createOperatorTable_ :: (MonadParser e s m) => (Tokens s -> expr) -> (expr -> expr -> expr) -> (expr -> expr -> expr -> expr) -> PrecedenceTable -> [[CombExpr.Operator m expr]]
createOperatorTable_ cVar cUn cBar = (fmap . fmap) (createOperator_ cVar cUn cBar) . groupSortOn (negate . precedence . snd)
{-# INLINEABLE createOperatorTable_ #-}

createOperator_ :: (MonadParser e s m) => (Tokens s -> expr) -> (expr -> expr -> expr) -> (expr -> expr -> expr -> expr) -> PrecedenceTableEntry -> CombExpr.Operator m expr
createOperator_ cVar _ cBin (op, PInfixN _) = CombExpr.InfixN (createOperatorBinParser_ cVar cBin op)
createOperator_ cVar _ cBin (op, PInfixL _) = CombExpr.InfixL (createOperatorBinParser_ cVar cBin op)
createOperator_ cVar _ cBin (op, PInfixR _) = CombExpr.InfixR (createOperatorBinParser_ cVar cBin op)
createOperator_ cVar cUn _ (op, PPrefix _) = CombExpr.Prefix (createOperatorUnParser_ cVar cUn op)
createOperator_ cVar cUn _ (op, PPostfix _) = CombExpr.Postfix (createOperatorUnParser_ cVar cUn op)

createOperatorBinParser_ :: (MonadParser e s m) => (Tokens s -> expr) -> (expr -> expr -> expr -> expr) -> String -> m (expr -> expr -> expr)
createOperatorBinParser_ cVar cBin op = (L.symbol op <?> "binary operator") $> cBin (cVar op)
{-# INLINEABLE createOperatorBinParser_ #-}

createOperatorUnParser_ :: (MonadParser e s m) => (Tokens s -> expr) -> (expr -> expr -> expr) -> String -> m (expr -> expr)
createOperatorUnParser_ cVar cUn op = (L.symbol op <?> "unary operator") $> cUn (cVar op)
{-# INLINEABLE createOperatorUnParser_ #-}
