{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
module Minicute.Types.Minicute.Expression
  ( module Minicute.Data.Fix


  , Identifier


  , IsRecursive( .. )
  , pattern Recursive
  , pattern NonRecursive


  , LetDefinition#

  , LetDefinition
  , MainLetDefinition

  , LetDefinitionL
  , MainLetDefinitionL

  , letDefinitionBinder
  , letDefinitionBody


  , MatchCase#

  , MatchCase
  , MainMatchCase

  , MatchCaseL
  , MainMatchCaseL

  , matchCaseTag
  , matchCaseArguments
  , matchCaseBody


  , Expression#( .. )

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


  , ExpressionL#( .. )

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


  , AnnotatedExpression#( .. )

  , AnnotatedExpression
  , pattern AnnotatedExpression
  , pattern AEInteger
  , pattern AEConstructor
  , pattern AEVariable
  , pattern AEApplication
  , pattern AEApplication2
  , pattern AEApplication3
  , pattern AELet
  , pattern AEMatch

  , annotation


  , AnnotatedExpressionL#( .. )

  , AnnotatedExpressionL
  , pattern AnnotatedExpressionL
  , pattern AELInteger
  , pattern AELConstructor
  , pattern AELVariable
  , pattern AELApplication
  , pattern AELApplication2
  , pattern AELApplication3
  , pattern AELLet
  , pattern AELMatch
  , pattern AELLambda

  , annotationL
  ) where

import Control.Lens
import GHC.Show ( appPrec, appPrec1 )
import Minicute.Data.Fix

type Identifier = String


newtype IsRecursive = IsRecursive { isRecursive :: Bool }
  deriving ( Eq
           )
pattern Recursive = IsRecursive True
pattern NonRecursive = IsRecursive False
{-# COMPLETE Recursive, NonRecursive #-}

instance Show IsRecursive where
  showsPrec _ Recursive = showString "Recursive"
  showsPrec _ NonRecursive = showString "NonRecursive"


type LetDefinition# expr_ a = (a, expr_ a)
type LetDefinition a = LetDefinition# Expression a
type MainLetDefinition = LetDefinition Identifier

type LetDefinitionL a = LetDefinition# ExpressionL a
type MainLetDefinitionL = LetDefinitionL Identifier

letDefinitionBinder :: Lens' (LetDefinition# expr_ a) a
letDefinitionBinder = _1

letDefinitionBody :: Lens (LetDefinition# expr_ a) (LetDefinition# expr_' a) (expr_ a) (expr_' a)
letDefinitionBody = _2


type MatchCase# expr_ a = (Int, [a], expr_ a)
type MatchCase a = MatchCase# Expression a
type MainMatchCase = MatchCase Identifier

type MatchCaseL a = MatchCase# ExpressionL a
type MainMatchCaseL = MatchCaseL Identifier

matchCaseTag :: Lens' (MatchCase# expr_ a) Int
matchCaseTag = _1

matchCaseArguments :: Lens' (MatchCase# expr_ a) [a]
matchCaseArguments = _2

matchCaseBody :: Lens (MatchCase# expr_ a) (MatchCase# expr_' a) (expr_ a) (expr_' a)
matchCaseBody = _3


data Expression# expr_ a
  = EInteger# Integer
  | EConstructor# Int Int
  | EVariable# Identifier
  | EApplication# (expr_ a) (expr_ a)
  | ELet# IsRecursive [LetDefinition# expr_ a] (expr_ a)
  | EMatch# (expr_ a) [MatchCase# expr_ a]
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
pattern ELet flag lds e = Fix2 (ELet# flag lds e)
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
  showsPrec p (ELet flag lds e)
    = showParen (p > appPrec)
      $ showString "ELet " . showsPrec appPrec1 flag . showString " " . showsPrec appPrec1 lds . showString " " . showsPrec appPrec1 e
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
pattern ELLet flag lds e = Fix2 (ELExpression# (ELet# flag lds e))
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
  showsPrec p (ELLet flag lds e)
    = showParen (p > appPrec)
      $ showString "ELLet " . showsPrec appPrec1 flag . showString " " . showsPrec appPrec1 lds . showString " " . showsPrec appPrec1 e
  showsPrec p (ELMatch e mcs)
    = showParen (p > appPrec)
      $ showString "ELMatch " . showsPrec appPrec1 e . showString " " . showsPrec appPrec1 mcs
  showsPrec p (ELLambda as e)
    = showParen (p > appPrec)
      $ showString "ELLambda " . showsPrec appPrec1 as . showString " " . showsPrec appPrec1 e


newtype AnnotatedExpression# ann expr_ a
  = AnnotatedExpression# (ann, Expression# expr_ a)
  deriving ( Eq
           , Show
           )

type AnnotatedExpression ann a = Fix2 (AnnotatedExpression# ann) a
pattern AnnotatedExpression ann expr = Fix2 (AnnotatedExpression# (ann, expr))
{-# COMPLETE AnnotatedExpression #-}
pattern AEInteger ann n = Fix2 (AnnotatedExpression# (ann, EInteger# n))
pattern AEConstructor ann tag args = Fix2 (AnnotatedExpression# (ann, EConstructor# tag args))
pattern AEVariable ann v = Fix2 (AnnotatedExpression# (ann, EVariable# v))
pattern AEApplication ann e1 e2 = Fix2 (AnnotatedExpression# (ann, EApplication# e1 e2))
pattern AEApplication2 ann2 ann1 e1 e2 e3 = AEApplication ann2 (AEApplication ann1 e1 e2) e3
pattern AEApplication3 ann3 ann2 ann1 e1 e2 e3 e4 = AEApplication ann3 (AEApplication2 ann2 ann1 e1 e2 e3) e4
pattern AELet ann flag lds e = Fix2 (AnnotatedExpression# (ann, ELet# flag lds e))
pattern AEMatch ann e mcs = Fix2 (AnnotatedExpression# (ann, EMatch# e mcs))
{-# COMPLETE AEInteger, AEConstructor, AEVariable, AEApplication, AELet, AEMatch #-}

instance {-# OVERLAPS #-} (Show ann, Show a) => Show (AnnotatedExpression ann a) where
  showsPrec p (AEInteger ann n)
    = showParen (p > appPrec)
      $ showString "AEInteger " . showsPrec appPrec1 ann . showString " " . showsPrec appPrec1 n
  showsPrec p (AEConstructor ann tag args)
    = showParen (p > appPrec)
      $ showString "AEConstructor " . showsPrec appPrec1 ann . showString " " . showsPrec appPrec1 tag . showString " " . showsPrec appPrec1 args
  showsPrec p (AEVariable ann v)
    = showParen (p > appPrec)
      $ showString "AEVariable " . showsPrec appPrec1 ann . showString " " . showsPrec appPrec1 v
  showsPrec p (AEApplication ann e1 e2)
    = showParen (p > appPrec)
      $ showString "AEApplication " . showsPrec appPrec1 ann . showString " " . showsPrec appPrec1 e1 . showString " " . showsPrec appPrec1 e2
  showsPrec p (AELet ann flag lds e)
    = showParen (p > appPrec)
      $ showString "AELet " . showsPrec appPrec1 ann . showString " " . showsPrec appPrec1 flag . showString " " . showsPrec appPrec1 lds . showString " " . showsPrec appPrec1 e
  showsPrec p (AEMatch ann e mcs)
    = showParen (p > appPrec)
      $ showString "AEMatch " . showsPrec appPrec1 ann . showString " " . showsPrec appPrec1 e . showString " " . showsPrec appPrec1 mcs

annotation :: Lens' (AnnotatedExpression ann a) ann
annotation = lens getter setter
  where
    getter (AnnotatedExpression ann _) = ann
    setter (AnnotatedExpression _ expr) ann = AnnotatedExpression ann expr


newtype AnnotatedExpressionL# ann expr_ a
  = AnnotatedExpressionL# (ann, ExpressionL# expr_ a)
  deriving ( Eq
           , Show
           )

type AnnotatedExpressionL ann a = Fix2 (AnnotatedExpressionL# ann) a
pattern AnnotatedExpressionL ann expr = Fix2 (AnnotatedExpressionL# (ann, expr))
{-# COMPLETE AnnotatedExpressionL #-}
pattern AELInteger ann n = Fix2 (AnnotatedExpressionL# (ann, ELExpression# (EInteger# n)))
pattern AELConstructor ann tag args = Fix2 (AnnotatedExpressionL# (ann, ELExpression# (EConstructor# tag args)))
pattern AELVariable ann v = Fix2 (AnnotatedExpressionL# (ann, ELExpression# (EVariable# v)))
pattern AELApplication ann e1 e2 = Fix2 (AnnotatedExpressionL# (ann, ELExpression# (EApplication# e1 e2)))
pattern AELApplication2 ann2 ann1 e1 e2 e3 = AELApplication ann2 (AELApplication ann1 e1 e2) e3
pattern AELApplication3 ann3 ann2 ann1 e1 e2 e3 e4 = AELApplication ann3 (AELApplication2 ann2 ann1 e1 e2 e3) e4
pattern AELLet ann flag lds e = Fix2 (AnnotatedExpressionL# (ann, ELExpression# (ELet# flag lds e)))
pattern AELMatch ann e mcs = Fix2 (AnnotatedExpressionL# (ann, ELExpression# (EMatch# e mcs)))
pattern AELLambda ann as e = Fix2 (AnnotatedExpressionL# (ann, ELLambda# as e))
{-# COMPLETE AELInteger, AELConstructor, AELVariable, AELApplication, AELLet, AELMatch, AELLambda #-}

instance {-# OVERLAPS #-} (Show ann, Show a) => Show (AnnotatedExpressionL ann a) where
  showsPrec p (AELInteger ann n)
    = showParen (p > appPrec)
      $ showString "AELInteger " . showsPrec appPrec1 ann . showString " " . showsPrec appPrec1 n
  showsPrec p (AELConstructor ann tag args)
    = showParen (p > appPrec)
      $ showString "AELConstructor " . showsPrec appPrec1 ann . showString " " . showsPrec appPrec1 tag . showString " " . showsPrec appPrec1 args
  showsPrec p (AELVariable ann v)
    = showParen (p > appPrec)
      $ showString "AELVariable " . showsPrec appPrec1 ann . showString " " . showsPrec appPrec1 v
  showsPrec p (AELApplication ann e1 e2)
    = showParen (p > appPrec)
      $ showString "AELApplication " . showsPrec appPrec1 ann . showString " " . showsPrec appPrec1 e1 . showString " " . showsPrec appPrec1 e2
  showsPrec p (AELLet ann flag lds e)
    = showParen (p > appPrec)
      $ showString "AELLet " . showsPrec appPrec1 ann . showString " " . showsPrec appPrec1 flag . showString " " . showsPrec appPrec1 lds . showString " " . showsPrec appPrec1 e
  showsPrec p (AELMatch ann e mcs)
    = showParen (p > appPrec)
      $ showString "AELMatch " . showsPrec appPrec1 ann . showString " " . showsPrec appPrec1 e . showString " " . showsPrec appPrec1 mcs
  showsPrec p (AELLambda ann as e)
    = showParen (p > appPrec)
      $ showString "AELLambda " . showsPrec appPrec1 ann . showString " " . showsPrec appPrec1 as . showString " " . showsPrec appPrec1 e

annotationL :: Lens' (AnnotatedExpressionL ann a) ann
annotationL = lens getter setter
  where
    getter (AnnotatedExpressionL ann _) = ann
    setter (AnnotatedExpressionL _ expr) ann = AnnotatedExpressionL ann expr
