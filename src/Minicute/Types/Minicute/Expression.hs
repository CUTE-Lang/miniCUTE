{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE PatternSynonyms #-}
module Minicute.Types.Minicute.Expression
  ( module Minicute.Data.Fix


  , Identifier


  , IsRecursive( .. )
  , pattern Recursive
  , pattern NonRecursive


  , LetDefinition_

  , LetDefinition
  , MainLetDefinition

  , LetDefinitionL
  , MainLetDefinitionL

  , _letDefinitionBinder
  , _letDefinitionBody


  , MatchCase_

  , MatchCase
  , MainMatchCase

  , MatchCaseL
  , MainMatchCaseL

  , _matchCaseTag
  , _matchCaseArguments
  , _matchCaseBody


  , Expression_( .. )

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


  , ExpressionL_( .. )

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
  , pattern ELExpression


  , AnnotatedExpression_( .. )

  , AnnotatedExpression
  , MainAnnotatedExpression
  , pattern AnnotatedExpression
  , pattern AEInteger
  , pattern AEConstructor
  , pattern AEVariable
  , pattern AEApplication
  , pattern AEApplication2
  , pattern AEApplication3
  , pattern AELet
  , pattern AEMatch

  , AnnotatedExpressionL
  , MainAnnotatedExpressionL
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
  , pattern AELExpression

  , _annotation
  ) where

import Control.Lens.Lens ( lens )
import Control.Lens.Tuple
import Control.Lens.Type
import Data.Data
import GHC.Generics
import GHC.Show ( appPrec, appPrec1 )
import Minicute.Data.Fix

type Identifier = String


newtype IsRecursive = IsRecursive { isRecursive :: Bool }
  deriving ( Generic
           , Typeable
           , Data
           , Eq
           , Ord
           )
pattern Recursive = IsRecursive True
pattern NonRecursive = IsRecursive False
{-# COMPLETE Recursive, NonRecursive #-}

instance Show IsRecursive where
  showsPrec _ Recursive = showString "Recursive"
  showsPrec _ NonRecursive = showString "NonRecursive"


type LetDefinition_ expr_ a = (a, expr_ a)
type LetDefinition a = LetDefinition_ Expression a
type MainLetDefinition = LetDefinition Identifier

type LetDefinitionL a = LetDefinition_ ExpressionL a
type MainLetDefinitionL = LetDefinitionL Identifier

_letDefinitionBinder :: Lens' (LetDefinition_ expr_ a) a
_letDefinitionBinder = _1
{-# INLINEABLE _letDefinitionBinder #-}

_letDefinitionBody :: Lens (LetDefinition_ expr_ a) (LetDefinition_ expr_' a) (expr_ a) (expr_' a)
_letDefinitionBody = _2
{-# INLINEABLE _letDefinitionBody #-}


type MatchCase_ expr_ a = (Int, [a], expr_ a)
type MatchCase a = MatchCase_ Expression a
type MainMatchCase = MatchCase Identifier

type MatchCaseL a = MatchCase_ ExpressionL a
type MainMatchCaseL = MatchCaseL Identifier

_matchCaseTag :: Lens' (MatchCase_ expr_ a) Int
_matchCaseTag = _1
{-# INLINEABLE _matchCaseTag #-}

_matchCaseArguments :: Lens' (MatchCase_ expr_ a) [a]
_matchCaseArguments = _2
{-# INLINEABLE _matchCaseArguments #-}

_matchCaseBody :: Lens (MatchCase_ expr_ a) (MatchCase_ expr_' a) (expr_ a) (expr_' a)
_matchCaseBody = _3
{-# INLINEABLE _matchCaseBody #-}


data Expression_ expr_ a
  = EInteger_ Integer
  | EConstructor_ Int Int
  | EVariable_ Identifier
  | EApplication_ (expr_ a) (expr_ a)
  | ELet_ IsRecursive [LetDefinition_ expr_ a] (expr_ a)
  | EMatch_ (expr_ a) [MatchCase_ expr_ a]
  deriving ( Generic
           , Typeable
           , Data
           , Eq
           , Ord
           , Show
           )

type Expression = Fix2 Expression_
type MainExpression = Expression Identifier
pattern EInteger n = Fix2 (EInteger_ n)
pattern EConstructor tag args = Fix2 (EConstructor_ tag args)
pattern EVariable v = Fix2 (EVariable_ v)
pattern EApplication e1 e2 = Fix2 (EApplication_ e1 e2)
pattern EApplication2 e1 e2 e3 = EApplication (EApplication e1 e2) e3
pattern EApplication3 e1 e2 e3 e4 = EApplication (EApplication2 e1 e2 e3) e4
pattern ELet flag lds e = Fix2 (ELet_ flag lds e)
pattern EMatch e mcs = Fix2 (EMatch_ e mcs)
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


data ExpressionL_ expr_ a
  = ELExpression_ (Expression_ expr_ a)
  | ELLambda_ [a] (expr_ a)
  deriving ( Generic
           , Typeable
           , Data
           , Eq
           , Ord
           , Show
           )

type ExpressionL = Fix2 ExpressionL_
type MainExpressionL = ExpressionL Identifier
pattern ELInteger n = ELExpression (EInteger_ n)
pattern ELConstructor tag args = ELExpression (EConstructor_ tag args)
pattern ELVariable v = ELExpression (EVariable_ v)
pattern ELApplication e1 e2 = ELExpression (EApplication_ e1 e2)
pattern ELApplication2 e1 e2 e3 = ELApplication (ELApplication e1 e2) e3
pattern ELApplication3 e1 e2 e3 e4 = ELApplication (ELApplication2 e1 e2 e3) e4
pattern ELLet flag lds e = ELExpression (ELet_ flag lds e)
pattern ELMatch e mcs = ELExpression (EMatch_ e mcs)
pattern ELLambda as e = Fix2 (ELLambda_ as e)
{-# COMPLETE ELInteger, ELConstructor, ELVariable, ELApplication, ELLet, ELMatch, ELLambda #-}
pattern ELExpression e = Fix2 (ELExpression_ e)
{-# COMPLETE ELExpression, ELLambda #-}

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


newtype AnnotatedExpression_ ann wExpr (expr_ :: * -> *) a
  = AnnotatedExpression_ (ann, wExpr expr_ a)
  deriving ( Generic
           , Typeable
           , Data
           , Eq
           , Ord
           , Show
           )

type AnnotatedExpression ann = Fix2 (AnnotatedExpression_ ann Expression_)
type MainAnnotatedExpression ann = AnnotatedExpression ann Identifier
pattern AnnotatedExpression ann expr = Fix2 (AnnotatedExpression_ (ann, expr))
{-# COMPLETE AnnotatedExpression #-}
pattern AEInteger ann n = AnnotatedExpression ann (EInteger_ n)
pattern AEConstructor ann tag args = AnnotatedExpression ann (EConstructor_ tag args)
pattern AEVariable ann v = AnnotatedExpression ann (EVariable_ v)
pattern AEApplication ann e1 e2 = AnnotatedExpression ann (EApplication_ e1 e2)
pattern AEApplication2 ann2 ann1 e1 e2 e3 = AEApplication ann2 (AEApplication ann1 e1 e2) e3
pattern AEApplication3 ann3 ann2 ann1 e1 e2 e3 e4 = AEApplication ann3 (AEApplication2 ann2 ann1 e1 e2 e3) e4
pattern AELet ann flag lds e = AnnotatedExpression ann (ELet_ flag lds e)
pattern AEMatch ann e mcs = AnnotatedExpression ann (EMatch_ e mcs)
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

type AnnotatedExpressionL ann = Fix2 (AnnotatedExpression_ ann ExpressionL_)
type MainAnnotatedExpressionL ann = AnnotatedExpressionL ann Identifier
pattern AnnotatedExpressionL ann expr = Fix2 (AnnotatedExpression_ (ann, expr))
{-# COMPLETE AnnotatedExpressionL #-}
pattern AELInteger ann n = AELExpression ann (EInteger_ n)
pattern AELConstructor ann tag args = AELExpression ann (EConstructor_ tag args)
pattern AELVariable ann v = AELExpression ann (EVariable_ v)
pattern AELApplication ann e1 e2 = AELExpression ann (EApplication_ e1 e2)
pattern AELApplication2 ann2 ann1 e1 e2 e3 = AELApplication ann2 (AELApplication ann1 e1 e2) e3
pattern AELApplication3 ann3 ann2 ann1 e1 e2 e3 e4 = AELApplication ann3 (AELApplication2 ann2 ann1 e1 e2 e3) e4
pattern AELLet ann flag lds e = AELExpression ann (ELet_ flag lds e)
pattern AELMatch ann e mcs = AELExpression ann (EMatch_ e mcs)
pattern AELLambda ann as e = AnnotatedExpressionL ann (ELLambda_ as e)
{-# COMPLETE AELInteger, AELConstructor, AELVariable, AELApplication, AELLet, AELMatch, AELLambda #-}
pattern AELExpression ann expr = AnnotatedExpressionL ann (ELExpression_ expr)
{-# COMPLETE AELExpression, AELLambda #-}

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

_annotation :: Lens (AnnotatedExpression_ ann wExpr expr_ a) (AnnotatedExpression_ ann' wExpr expr_ a) ann ann'
_annotation = lens getter setter
  where
    getter (AnnotatedExpression_ (ann, _)) = ann
    setter (AnnotatedExpression_ (_, expr)) ann = AnnotatedExpression_ (ann, expr)
{-# INLINEABLE _annotation #-}
