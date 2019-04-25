{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
module Minicute.Types.Minicute.Program
  ( module Minicute.Types.Minicute.Expression

  , Supercombinator#

  , Supercombinator
  , MainSupercombinator

  , SupercombinatorL
  , MainSupercombinatorL

  , AnnotatedSupercombinator

  , AnnotatedSupercombinatorL

  , supercombinatorBinder
  , supercombinatorArguments
  , supercombinatorBody


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
  ) where

import Control.Lens
import Data.Data
import GHC.Generics
import GHC.Show ( appPrec, appPrec1 )
import Minicute.Types.Minicute.Expression

type Supercombinator# a expr = (Identifier, [a], expr)

type Supercombinator a = Supercombinator# a (Expression a)
type MainSupercombinator = Supercombinator Identifier

type SupercombinatorL a = Supercombinator# a (ExpressionL a)
type MainSupercombinatorL = SupercombinatorL Identifier

type AnnotatedSupercombinator ann a = Supercombinator# a (AnnotatedExpression ann a)

type AnnotatedSupercombinatorL ann a = Supercombinator# a (AnnotatedExpressionL ann a)

supercombinatorBinder :: Lens' (Supercombinator# a expr) Identifier
supercombinatorBinder = _1
{-# INLINEABLE supercombinatorBinder #-}

supercombinatorArguments :: Lens' (Supercombinator# a expr) [a]
supercombinatorArguments = _2
{-# INLINEABLE supercombinatorArguments #-}

supercombinatorBody :: Lens (Supercombinator# a expr1) (Supercombinator# a expr2) expr1 expr2
supercombinatorBody = _3
{-# INLINEABLE supercombinatorBody #-}


newtype Program# a expr
  = Program# [Supercombinator# a expr]
  deriving ( Generic
           , Typeable
           , Data
           , Eq
           , Ord
           , Show
           )

type Program a = Program# a (Expression a)
type MainProgram = Program Identifier
pattern Program sc = Program# sc
{-# COMPLETE Program #-}

instance {-# OVERLAPS #-} (Show a) => Show (Program a) where
  showsPrec = showProgram# "Program "


type ProgramL a = Program# a (ExpressionL a)
type MainProgramL = ProgramL Identifier
pattern ProgramL sc = Program# sc
{-# COMPLETE ProgramL #-}

instance {-# OVERLAPS #-} (Show a) => Show (ProgramL a) where
  showsPrec = showProgram# "ProgramL "


type AnnotatedProgram ann a = Program# a (AnnotatedExpression ann a)
pattern AnnotatedProgram sc = Program# sc
{-# COMPLETE AnnotatedProgram #-}

instance {-# OVERLAPS #-} (Show ann, Show a) => Show (AnnotatedProgram ann a) where
  showsPrec = showProgram# "AnnotatedProgram "


type AnnotatedProgramL ann a = Program# a (AnnotatedExpressionL ann a)
pattern AnnotatedProgramL sc = Program# sc
{-# COMPLETE AnnotatedProgramL #-}

instance {-# OVERLAPS #-} (Show ann, Show a) => Show (AnnotatedProgramL ann a) where
  showsPrec = showProgram# "AnnotatedProgramL "

showProgram# :: (Show a, Show expr) => String -> Int -> Program# a expr -> ShowS
showProgram# name p (Program# scs)
  = showParen (p > appPrec)
    $ showString name . showsPrec appPrec1 scs
{-# INLINEABLE showProgram# #-}
