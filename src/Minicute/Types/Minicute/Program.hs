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

type Supercombinator_ a expr = (Identifier, [a], expr a)

type Supercombinator a = Supercombinator_ a Expression
type MainSupercombinator = Supercombinator Identifier

type SupercombinatorL a = Supercombinator_ a ExpressionL
type MainSupercombinatorL = SupercombinatorL Identifier

type AnnotatedSupercombinator ann a = Supercombinator_ a (AnnotatedExpression ann)

type AnnotatedSupercombinatorL ann a = Supercombinator_ a (AnnotatedExpressionL ann)

_supercombinatorBinder :: Lens' (Supercombinator_ a expr) Identifier
_supercombinatorBinder = _1
{-# INLINEABLE _supercombinatorBinder #-}

_supercombinatorArguments :: Lens' (Supercombinator_ a expr) [a]
_supercombinatorArguments = _2
{-# INLINEABLE _supercombinatorArguments #-}

_supercombinatorBody :: Lens (Supercombinator_ a expr1) (Supercombinator_ a expr2) (expr1 a) (expr2 a)
_supercombinatorBody = _3
{-# INLINEABLE _supercombinatorBody #-}


newtype Program_ a expr
  = Program_ [Supercombinator_ a expr]
  deriving ( Generic
           , Typeable
           , Data
           , Eq
           , Ord
           , Show
           )

type Program a = Program_ a Expression
type MainProgram = Program Identifier
pattern Program sc = Program_ sc
{-# COMPLETE Program #-}

instance {-# OVERLAPS #-} (Show a) => Show (Program a) where
  showsPrec = showProgram_ "Program "


type ProgramL a = Program_ a ExpressionL
type MainProgramL = ProgramL Identifier
pattern ProgramL sc = Program_ sc
{-# COMPLETE ProgramL #-}

instance {-# OVERLAPS #-} (Show a) => Show (ProgramL a) where
  showsPrec = showProgram_ "ProgramL "


type AnnotatedProgram ann a = Program_ a (AnnotatedExpression ann)
pattern AnnotatedProgram sc = Program_ sc
{-# COMPLETE AnnotatedProgram #-}

instance {-# OVERLAPS #-} (Show ann, Show a) => Show (AnnotatedProgram ann a) where
  showsPrec = showProgram_ "AnnotatedProgram "


type AnnotatedProgramL ann a = Program_ a (AnnotatedExpressionL ann)
pattern AnnotatedProgramL sc = Program_ sc
{-# COMPLETE AnnotatedProgramL #-}

instance {-# OVERLAPS #-} (Show ann, Show a) => Show (AnnotatedProgramL ann a) where
  showsPrec = showProgram_ "AnnotatedProgramL "

showProgram_ :: (Show a, Show (expr a)) => String -> Int -> Program_ a expr -> ShowS
showProgram_ name p (Program_ scs)
  = showParen (p > appPrec)
    $ showString name . showsPrec appPrec1 scs
{-# INLINEABLE showProgram_ #-}

_supercombinators :: Lens (Program_ a expr) (Program_ a' expr') [Supercombinator_ a expr] [Supercombinator_ a' expr']
_supercombinators = lens getter setter
  where
    getter (Program_ scs) = scs
    setter _ = Program_
{-# INLINEABLE _supercombinators #-}
