{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
module Minicute.Types.Minicute.Program
  ( module Minicute.Types.Minicute.Expression

  , Supercombinator#

  , Supercombinator
  , MainSupercombinator

  , SupercombinatorL
  , MainSupercombinatorL

  , AnnotatedSupercombinator

  , AnnotatedSupercombinatorL

  , _supercombinatorBinder
  , _supercombinatorArguments
  , _supercombinatorBody


  , Program#( .. )

  , Program
  , MainProgram
  , pattern Program


  , ProgramL
  , MainProgramL
  , pattern ProgramL


  , AnnotatedProgram
  , pattern AnnotatedProgram


  , AnnotatedProgramL
  , pattern AnnotatedProgramL


  , _supercombinators
  ) where

import Control.Lens
import Data.Data
import GHC.Generics
import GHC.Show ( appPrec, appPrec1 )
import Minicute.Types.Minicute.Expression

type Supercombinator# a expr = (Identifier, [a], expr a)

type Supercombinator a = Supercombinator# a Expression
type MainSupercombinator = Supercombinator Identifier

type SupercombinatorL a = Supercombinator# a ExpressionL
type MainSupercombinatorL = SupercombinatorL Identifier

type AnnotatedSupercombinator ann a = Supercombinator# a (AnnotatedExpression ann)

type AnnotatedSupercombinatorL ann a = Supercombinator# a (AnnotatedExpressionL ann)

_supercombinatorBinder :: Lens' (Supercombinator# a expr) Identifier
_supercombinatorBinder = _1
{-# INLINEABLE _supercombinatorBinder #-}

_supercombinatorArguments :: Lens' (Supercombinator# a expr) [a]
_supercombinatorArguments = _2
{-# INLINEABLE _supercombinatorArguments #-}

_supercombinatorBody :: Lens (Supercombinator# a expr1) (Supercombinator# a expr2) (expr1 a) (expr2 a)
_supercombinatorBody = _3
{-# INLINEABLE _supercombinatorBody #-}


newtype Program# a expr
  = Program# [Supercombinator# a expr]
  deriving ( Generic
           , Typeable
           , Data
           , Eq
           , Ord
           , Show
           )

type Program a = Program# a Expression
type MainProgram = Program Identifier
pattern Program sc = Program# sc
{-# COMPLETE Program #-}

instance {-# OVERLAPS #-} (Show a) => Show (Program a) where
  showsPrec = showProgram# "Program "


type ProgramL a = Program# a ExpressionL
type MainProgramL = ProgramL Identifier
pattern ProgramL sc = Program# sc
{-# COMPLETE ProgramL #-}

instance {-# OVERLAPS #-} (Show a) => Show (ProgramL a) where
  showsPrec = showProgram# "ProgramL "


type AnnotatedProgram ann a = Program# a (AnnotatedExpression ann)
pattern AnnotatedProgram sc = Program# sc
{-# COMPLETE AnnotatedProgram #-}

instance {-# OVERLAPS #-} (Show ann, Show a) => Show (AnnotatedProgram ann a) where
  showsPrec = showProgram# "AnnotatedProgram "


type AnnotatedProgramL ann a = Program# a (AnnotatedExpressionL ann)
pattern AnnotatedProgramL sc = Program# sc
{-# COMPLETE AnnotatedProgramL #-}

instance {-# OVERLAPS #-} (Show ann, Show a) => Show (AnnotatedProgramL ann a) where
  showsPrec = showProgram# "AnnotatedProgramL "

showProgram# :: (Show a, Show (expr a)) => String -> Int -> Program# a expr -> ShowS
showProgram# name p (Program# scs)
  = showParen (p > appPrec)
    $ showString name . showsPrec appPrec1 scs
{-# INLINEABLE showProgram# #-}

_supercombinators :: Lens (Program# a expr) (Program# a' expr') [Supercombinator# a expr] [Supercombinator# a' expr']
_supercombinators = lens getter setter
  where
    getter (Program# scs) = scs
    setter _ = Program#
{-# INLINEABLE _supercombinators #-}
