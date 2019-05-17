{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
module Minicute.Types.Minicute.Program
  ( module Minicute.Types.Minicute.Expression

  , Supercombinator_

  , Supercombinator
  , MainSupercombinator

  , SupercombinatorL
  , MainSupercombinatorL

  , AnnotatedSupercombinator

  , AnnotatedSupercombinatorL

  , _supercombinatorBinder
  , _supercombinatorArguments
  , _supercombinatorBody


  , Program_( .. )

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

import Control.Lens.Lens ( lens )
import Control.Lens.Tuple
import Control.Lens.Type
import Data.Data
import GHC.Generics
import GHC.Show ( appPrec, appPrec1 )
import Minicute.Types.Minicute.Expression

type Supercombinator_ expr a = (Identifier, [a], expr a)

type Supercombinator a = Supercombinator_ Expression a
type MainSupercombinator = Supercombinator Identifier

type SupercombinatorL a = Supercombinator_ ExpressionL a
type MainSupercombinatorL = SupercombinatorL Identifier

type AnnotatedSupercombinator ann a = Supercombinator_ (AnnotatedExpression ann) a

type AnnotatedSupercombinatorL ann a = Supercombinator_ (AnnotatedExpressionL ann) a

_supercombinatorBinder :: Lens' (Supercombinator_ expr a) Identifier
_supercombinatorBinder = _1
{-# INLINEABLE _supercombinatorBinder #-}

_supercombinatorArguments :: Lens' (Supercombinator_ expr a) [a]
_supercombinatorArguments = _2
{-# INLINEABLE _supercombinatorArguments #-}

_supercombinatorBody :: Lens (Supercombinator_ expr1 a) (Supercombinator_ expr2 a) (expr1 a) (expr2 a)
_supercombinatorBody = _3
{-# INLINEABLE _supercombinatorBody #-}


newtype Program_ expr a
  = Program_ [Supercombinator_ expr a]
  deriving ( Generic
           , Typeable
           , Data
           , Eq
           , Ord
           , Show
           )

type Program a = Program_ Expression a
type MainProgram = Program Identifier
pattern Program sc = Program_ sc
{-# COMPLETE Program #-}

instance {-# OVERLAPS #-} (Show a) => Show (Program a) where
  showsPrec = showProgram_ "Program "


type ProgramL a = Program_ ExpressionL a
type MainProgramL = ProgramL Identifier
pattern ProgramL sc = Program_ sc
{-# COMPLETE ProgramL #-}

instance {-# OVERLAPS #-} (Show a) => Show (ProgramL a) where
  showsPrec = showProgram_ "ProgramL "


type AnnotatedProgram ann a = Program_ (AnnotatedExpression ann) a
pattern AnnotatedProgram sc = Program_ sc
{-# COMPLETE AnnotatedProgram #-}

instance {-# OVERLAPS #-} (Show ann, Show a) => Show (AnnotatedProgram ann a) where
  showsPrec = showProgram_ "AnnotatedProgram "


type AnnotatedProgramL ann a = Program_ (AnnotatedExpressionL ann) a
pattern AnnotatedProgramL sc = Program_ sc
{-# COMPLETE AnnotatedProgramL #-}

instance {-# OVERLAPS #-} (Show ann, Show a) => Show (AnnotatedProgramL ann a) where
  showsPrec = showProgram_ "AnnotatedProgramL "

showProgram_ :: (Show a, Show (expr a)) => String -> Int -> Program_ expr a -> ShowS
showProgram_ name p (Program_ scs)
  = showParen (p > appPrec)
    $ showString name . showsPrec appPrec1 scs
{-# INLINEABLE showProgram_ #-}

_supercombinators :: Lens (Program_ expr a) (Program_ expr' a') [Supercombinator_ expr a] [Supercombinator_ expr' a']
_supercombinators = lens getter setter
  where
    getter (Program_ scs) = scs
    setter _ = Program_
{-# INLINEABLE _supercombinators #-}
