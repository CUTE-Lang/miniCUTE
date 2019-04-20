{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Minicute.Parser.Types
  ( MonadParser

  , WithPrecedence

  , Parser

  , Precedence( .. )
  , PrecedenceTableEntry
  , PrecedenceTable
  , defaultPrecedenceTable

  , Operator
  , OperatorTable
  ) where

import Control.Monad.Reader ( ReaderT )
import Data.Void
import Minicute.Parser.Precedence
import Minicute.Types.Minicute.Program
import Text.Megaparsec

import qualified Control.Monad.Combinators.Expr as CombExpr

type MonadParser e s m = (MonadParsec e s m, ShowErrorComponent e, s ~ String)

type WithPrecedence m = ReaderT PrecedenceTable m

type Parser = Parsec Void String

type Operator m = CombExpr.Operator m MainExpressionL
type OperatorTable m = [[Operator m]]
