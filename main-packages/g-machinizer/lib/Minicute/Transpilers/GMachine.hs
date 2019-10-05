-- |
-- Transpilers from a miniCUTE program to a G-Machine program
module Minicute.Transpilers.GMachine
  ( module Minicute.Data.GMachine.Instruction
  , module Minicute.Data.Minicute.Program
  , transpileProgram

  , initialCode
  ) where

import Minicute.Data.GMachine.Instruction
import Minicute.Data.Minicute.Program
import Minicute.Internal.Transpilers.GMachine
