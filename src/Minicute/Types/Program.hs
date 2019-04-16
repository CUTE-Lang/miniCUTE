{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
module Minicute.Types.Program
  ( module Minicute.Types.Expression

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

import Minicute.Types.Expression

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

type ProgramL a = Program# a (ExpressionL a)
type MainProgramL = ProgramL Identifier

pattern ProgramL sc = Program# sc
{-# COMPLETE ProgramL #-}
