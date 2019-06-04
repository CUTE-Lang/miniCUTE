module Minicute.Types.GMachine.Instruction where

import Control.Lens.Operators
import Minicute.Types.Minicute.Program

-- |
-- Which calling convention we gonna use?

transpileProgram :: MainProgram -> [(Identifier, [Instruction])]
transpileProgram program = program ^. _supercombinators <&> transpileSc

transpileSc :: MainSupercombinator -> (Identifier, [Instruction])
transpileSc sc = (scBinder, scBodyInstruction)
  where
    scBinder = sc ^. _supercombinatorBinder
    scBodyInstruction = sc ^. _supercombinatorBody & transpileE

transpileE :: MainExpression -> [Instruction]
transpileE (EInteger n) = [IMakeInteger n]
transpileE (EConstructor tag arity) = [IMakeConstructor tag arity]
transpileE (EVariable _) = error "Require environment of compilation"
transpileE (EApplication e1 e2)
  = transpileE e1 <> transpileE e2 <> [IMakeApplication]
transpileE (ELet _ _ _) = error "Require environment of compilation"
transpileE (EMatch _ _) = error "Require environment of compilation"
transpileE _ = error "Not implemented yet"

data Instruction
  -- |
  -- Operations that modify stack and heap
  = IMakeInteger Integer
  | IMakeConstructor Int Int
  | IMakeApplication
  deriving ( Eq
           , Show
           )