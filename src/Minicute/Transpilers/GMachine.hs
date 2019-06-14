{-# OPTIONS_HADDOCK not-home #-}
-- |
-- Transpilers from a miniCUTE program to a G-Machine program
module Minicute.Transpilers.GMachine
  ( module Minicute.Types.GMachine.Instruction
  , module Minicute.Types.Minicute.Program
  , transpileProgram

  , initialCode
  ) where

import Minicute.Internal.Transpilers.GMachine
import Minicute.Types.GMachine.Instruction
import Minicute.Types.Minicute.Program
