module Minicute.Transpilers.Generator
  ( module Minicute.Data.GMachine.Instruction
  , generateMachineCode
  ) where

import Minicute.Data.GMachine.Instruction

import qualified LLVM.AST as AST

generateMachineCode :: GMachineProgram -> [AST.Definition]
generateMachineCode _ = []
