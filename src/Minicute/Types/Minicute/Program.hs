{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}
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

  , Program#

  , Program
  , MainProgram
  , pattern Program

  , ProgramL
  , MainProgramL
  , pattern ProgramL
  ) where

import GHC.Show (appPrec, appPrec1)
import Minicute.Types.Minicute.Expression

type Supercombinator# a expr = (Identifier, [a], expr)

type Supercombinator a = Supercombinator# a (Expression a)
type MainSupercombinator = Supercombinator Identifier

type SupercombinatorL a = Supercombinator# a (ExpressionL a)
type MainSupercombinatorL = SupercombinatorL Identifier

newtype Program# a expr
  = Program# [Supercombinator# a expr]
  deriving ( Eq
           , Show
           )
type Program a = Program# a (Expression a)
type MainProgram = Program Identifier

pattern Program sc = Program# sc
{-# COMPLETE Program #-}

instance {-# OVERLAPS #-} (Show a) => Show (Program a) where
  showsPrec p (Program sc)
    = showParen (p > appPrec)
    $ showString "Program " . showsPrec appPrec1 sc

type ProgramL a = Program# a (ExpressionL a)
type MainProgramL = ProgramL Identifier

pattern ProgramL sc = Program# sc
{-# COMPLETE ProgramL #-}

instance {-# OVERLAPS #-} (Show a) => Show (ProgramL a) where
  showsPrec p (ProgramL sc)
    = showParen (p > appPrec)
    $ showString "ProgramL " . showsPrec appPrec1 sc
