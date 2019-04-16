module Minicute.Types.Program
  ( module Minicute.Types.Expression

  , SuperCombinator
  , MainSuperCombinator

  , SuperCombinatorL
  , MainSuperCombinatorL

  , Program( .. )
  , MainProgram

  , ProgramL( .. )
  , MainProgramL
  ) where

import Minicute.Types.Expression

type SuperCombinator a = (Identifier, [a], Expression a)
type MainSuperCombinator = SuperCombinator Identifier

type SuperCombinatorL a = (Identifier, [a], ExpressionL a)
type MainSuperCombinatorL = SuperCombinatorL Identifier

newtype Program a
  = Program [SuperCombinator a]
  deriving ( Eq
           , Show
           )
type MainProgram = Program Identifier

newtype ProgramL a
  = ProgramL [SuperCombinatorL a]
  deriving ( Eq
           , Show
           )
type MainProgramL = ProgramL Identifier
