{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
module Minicute.Types.Expression
  ( module Minicute.Data.Fix

  , Identifier
  , IsRec
  , MatchCase
  , LetDefinition

  , Expression#
  , Expression
  , MainExpression
  , pattern ENum
  , pattern EConstr
  , pattern EVar
  , pattern EAp
  , pattern ELet
  , pattern EMatch

  , ExpressionL#
  , ExpressionL
  , MainExpressionL
  , pattern ELNum
  , pattern ELConstr
  , pattern ELVar
  , pattern ELAp
  , pattern ELLet
  , pattern ELMatch
  , pattern ELLam
  ) where

import Minicute.Data.Fix

type Identifier = String
type IsRec = Bool
type MatchCase expr_ a = (Int, [a], expr_ a)
type LetDefinition expr_ a = (a, expr_ a)

data Expression# expr_ a
  = ENum# Integer
  | EConstr# Int Int
  | EVar# Identifier
  | EAp# (expr_ a) (expr_ a)
  | ELet# IsRec [LetDefinition expr_ a] (expr_ a)
  | EMatch# (expr_ a) [MatchCase expr_ a]
  deriving ( Eq
           , Show
           )
type Expression a = Fix2 Expression# a
type MainExpression = Expression Identifier

pattern ENum n <- Fix2 (ENum# n) where
  ENum n = Fix2 (ENum# n)
pattern EConstr tag args = Fix2 (EConstr# tag args)
pattern EVar v = Fix2 (EVar# v)
pattern EAp e1 e2 = Fix2 (EAp# e1 e2)
pattern ELet ir lds e = Fix2 (ELet# ir lds e)
pattern EMatch e mcs = Fix2 (EMatch# e mcs)
{-# COMPLETE ENum, EConstr, EVar, EAp, ELet, EMatch #-}

data ExpressionL# expr_ a
  = ELExpr# (Expression# expr_ a)
  | ELLam# [a] (expr_ a)
  deriving ( Eq
           , Show
           )
type ExpressionL a = Fix2 ExpressionL# a
type MainExpressionL = ExpressionL Identifier

pattern ELNum n = Fix2 (ELExpr# (ENum# n))
pattern ELConstr tag args = Fix2 (ELExpr# (EConstr# tag args))
pattern ELVar v = Fix2 (ELExpr# (EVar# v))
pattern ELAp e1 e2 = Fix2 (ELExpr# (EAp# e1 e2))
pattern ELLet ir lds e = Fix2 (ELExpr# (ELet# ir lds e))
pattern ELMatch e mcs = Fix2 (ELExpr# (EMatch# e mcs))
pattern ELLam as e = Fix2 (ELLam# as e)
{-# COMPLETE ELNum, ELConstr, ELVar, ELAp, ELLet, ELMatch, ELLam #-}
