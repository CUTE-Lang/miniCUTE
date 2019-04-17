{-# LANGUAGE TypeFamilies #-}
module Minicute.Parser.Parser
  ( Parser

  , MainProgramL
  , programL
  ) where

import Control.Monad.Reader
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

precedenceTable :: Parser PrecedenceTable
precedenceTable = return defaultPrecedenceTable

supercombinatorL :: ParserWithPrecedence MainSupercombinatorL
supercombinatorL = (,,) <$> L.identifier <*> (many L.identifier <* L.symbol "=") <*> expressionL

expressionL :: ParserWithPrecedence MainExpressionL
expressionL
  = otherExpressionsByPrec
    <?> "expression"

otherExpressionsByPrec :: ParserWithPrecedence MainExpressionL
otherExpressionsByPrec = ask >>= CombExpr.makeExprParser applicationExpressionL . precedenceTableToOperatorTable

applicationExpressionL :: ParserWithPrecedence MainExpressionL
applicationExpressionL
  = makeApplicationChain <$> CombNE.some atomicExpressionL
  where
    makeApplicationChain (aExpr :| aExprs) = foldl' ELApplication aExpr aExprs

atomicExpressionL :: ParserWithPrecedence MainExpressionL
atomicExpressionL
  = integerExpression
    <|> constructorExpression
    <|> variableExpression
    <|> mapReaderT L.betweenRoundBrackets expressionL

integerExpression :: (MonadParsec e s m, s ~ String) => m MainExpressionL
integerExpression = ELInteger <$> L.integer

variableExpression :: (MonadParsec e s m, s ~ String) => m MainExpressionL
variableExpression = ELVariable <$> L.identifier

constructorExpression :: (MonadParsec e s m, s ~ String) => m MainExpressionL
constructorExpression = L.string "Pack" *> L.string "{" *> (ELConstructor <$> (L.integer <* L.string ",") <*> L.integer) <* L.string "}"

precedenceTableToOperatorTable :: PrecedenceTable -> OperatorTable
precedenceTableToOperatorTable = fmap (fmap precedenceTableEntryToOperator) . groupSortOn (negate . precedence . snd)

type Operator = CombExpr.Operator ParserWithPrecedence MainExpressionL
type OperatorTable = [[Operator]]

precedenceTableEntryToOperator :: PrecedenceTableEntry -> Operator
precedenceTableEntryToOperator (op, PInfixN _) = CombExpr.InfixN (lift (L.string op $> ELApplication2 (ELVariable op)))
precedenceTableEntryToOperator (op, PInfixL _) = CombExpr.InfixL (lift (L.string op $> ELApplication2 (ELVariable op)))
precedenceTableEntryToOperator (op, PInfixR _) = CombExpr.InfixR (lift (L.string op $> ELApplication2 (ELVariable op)))
precedenceTableEntryToOperator (op, PPrefix _) = CombExpr.Prefix (lift (L.string op $> ELApplication (ELVariable op)))
precedenceTableEntryToOperator (op, PPostfix _) = CombExpr.Postfix (lift (L.string op $> ELApplication (ELVariable op)))
