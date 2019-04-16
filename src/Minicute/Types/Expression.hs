{-# LANGUAGE MagicHash #-}
module Minicute.Types.Expression
  ( module Minicute.Data.Fix

  , Identifier
  , IsRec
  , MatchCase
  , LetDefinition

  , Expression#(..)
  , Expression
  , MainExpression

  , ExpressionL#(..)
  , ExpressionL
  , MainExpressionL
  ) where

import Minicute.Data.Fix

type Identifier = String
type IsRec = Bool
type MatchCase expr a = (Int, [a], expr a)
type LetDefinition expr a = (a, expr a)

data Expression# expr a
  = ENum Integer
  | EConstr Int Int
  | EVar Identifier
  | EAp (expr a) (expr a)
  | ELet IsRec [LetDefinition expr a] (expr a)
  | EMatch (expr a) [MatchCase expr a]
  deriving ( Eq
           , Show
           )
type Expression a = Fix2 Expression# a
type MainExpression = Expression Identifier

data ExpressionL# expr a
  = ELExpr (Expression# expr a)
  | ELLam [a] (expr a)
  deriving ( Eq
           , Show
           )
type ExpressionL a = Fix2 ExpressionL# a
type MainExpressionL = ExpressionL Identifier
