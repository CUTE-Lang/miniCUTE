{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
module Minicute.Types.Minicute.Expression
  ( module Minicute.Data.Fix

  , Identifier
  , IsRecursive
  , MatchCase
  , LetDefinition

  , Expression#
  , Expression
  , MainExpression
  , pattern EInteger
  , pattern EConstructor
  , pattern EVariable
  , pattern EApplication
  , pattern EApplication2
  , pattern EApplication3
  , pattern ELet
  , pattern EMatch

  , ExpressionL#
  , ExpressionL
  , MainExpressionL
  , pattern ELInteger
  , pattern ELConstructor
  , pattern ELVariable
  , pattern ELApplication
  , pattern ELApplication2
  , pattern ELApplication3
  , pattern ELLet
  , pattern ELMatch
  , pattern ELLambda
  ) where

import GHC.Show (appPrec, appPrec1)
import Minicute.Data.Fix

type Identifier = String
type IsRecursive = Bool
type MatchCase expr_ a = (Int, [a], expr_ a)
type LetDefinition expr_ a = (a, expr_ a)

data Expression# expr_ a
  = EInteger# Integer
  | EConstructor# Int Int
  | EVariable# Identifier
  | EApplication# (expr_ a) (expr_ a)
  | ELet# IsRecursive [LetDefinition expr_ a] (expr_ a)
  | EMatch# (expr_ a) [MatchCase expr_ a]
  deriving ( Eq
           , Show
           )
type Expression a = Fix2 Expression# a
type MainExpression = Expression Identifier

pattern EInteger n = Fix2 (EInteger# n)
pattern EConstructor tag args = Fix2 (EConstructor# tag args)
pattern EVariable v = Fix2 (EVariable# v)
pattern EApplication e1 e2 = Fix2 (EApplication# e1 e2)
pattern EApplication2 e1 e2 e3 = EApplication (EApplication e1 e2) e3
pattern EApplication3 e1 e2 e3 e4 = EApplication (EApplication2 e1 e2 e3) e4
pattern ELet ir lds e = Fix2 (ELet# ir lds e)
pattern EMatch e mcs = Fix2 (EMatch# e mcs)
{-# COMPLETE EInteger, EConstructor, EVariable, EApplication, ELet, EMatch #-}

instance {-# OVERLAPS #-} (Show a) => Show (Expression a) where
  showsPrec p (EInteger n)
    = showParen (p > appPrec)
      $ showString "EInteger " . showsPrec appPrec1 n
  showsPrec p (EConstructor tag args)
    = showParen (p > appPrec)
      $ showString "EConstructor " . showsPrec appPrec1 tag . showString " " . showsPrec appPrec1 args
  showsPrec p (EVariable v)
    = showParen (p > appPrec)
      $ showString "EVariable " . showsPrec appPrec1 v
  showsPrec p (EApplication e1 e2)
    = showParen (p > appPrec)
      $ showString "EApplication " . showsPrec appPrec1 e1 . showString " " . showsPrec appPrec1 e2
  showsPrec p (ELet ir lds e)
    = showParen (p > appPrec)
      $ showString "ELet " . showsPrec appPrec1 ir . showString " " . showsPrec appPrec1 lds . showString " " . showsPrec appPrec1 e
  showsPrec p (EMatch e mcs)
    = showParen (p > appPrec)
      $ showString "EMatch " . showsPrec appPrec1 e . showString " " . showsPrec appPrec1 mcs

data ExpressionL# expr_ a
  = ELExpression# (Expression# expr_ a)
  | ELLambda# [a] (expr_ a)
  deriving ( Eq
           , Show
           )
type ExpressionL a = Fix2 ExpressionL# a
type MainExpressionL = ExpressionL Identifier

pattern ELInteger n = Fix2 (ELExpression# (EInteger# n))
pattern ELConstructor tag args = Fix2 (ELExpression# (EConstructor# tag args))
pattern ELVariable v = Fix2 (ELExpression# (EVariable# v))
pattern ELApplication e1 e2 = Fix2 (ELExpression# (EApplication# e1 e2))
pattern ELApplication2 e1 e2 e3 = ELApplication (ELApplication e1 e2) e3
pattern ELApplication3 e1 e2 e3 e4 = ELApplication (ELApplication2 e1 e2 e3) e4
pattern ELLet ir lds e = Fix2 (ELExpression# (ELet# ir lds e))
pattern ELMatch e mcs = Fix2 (ELExpression# (EMatch# e mcs))
pattern ELLambda as e = Fix2 (ELLambda# as e)
{-# COMPLETE ELInteger, ELConstructor, ELVariable, ELApplication, ELLet, ELMatch, ELLambda #-}

instance {-# OVERLAPS #-} (Show a) => Show (ExpressionL a) where
  showsPrec p (ELInteger n)
    = showParen (p > appPrec)
      $ showString "ELInteger " . showsPrec appPrec1 n
  showsPrec p (ELConstructor tag args)
    = showParen (p > appPrec)
      $ showString "ELConstructor " . showsPrec appPrec1 tag . showString " " . showsPrec appPrec1 args
  showsPrec p (ELVariable v)
    = showParen (p > appPrec)
      $ showString "ELVariable " . showsPrec appPrec1 v
  showsPrec p (ELApplication e1 e2)
    = showParen (p > appPrec)
      $ showString "ELApplication " . showsPrec appPrec1 e1 . showString " " . showsPrec appPrec1 e2
  showsPrec p (ELLet ir lds e)
    = showParen (p > appPrec)
      $ showString "ELLet " . showsPrec appPrec1 ir . showString " " . showsPrec appPrec1 lds . showString " " . showsPrec appPrec1 e
  showsPrec p (ELMatch e mcs)
    = showParen (p > appPrec)
      $ showString "ELMatch " . showsPrec appPrec1 e . showString " " . showsPrec appPrec1 mcs
  showsPrec p (ELLambda as e)
    = showParen (p > appPrec)
      $ showString "ELLambda " . showsPrec appPrec1 as . showString " " . showsPrec appPrec1 e
