{-# LANGUAGE NamedFieldPuns #-}
module Minicute.Interpreter.GMachine
  ( interpretProgram
  ) where

import Minicute.Data.GMachine.Instruction
import Minicute.Interpreter.GMachine.Monad
import Minicute.Interpreter.GMachine.Common

interpretProgram :: GMachineProgram -> InterpreterMonad ()
interpretProgram program
  = initializeInterpreterWith program >> go
  where
    go = fetchNextInstruction >>= interpretInstruction >> go

interpretInstruction :: Instruction -> InterpreterMonad ()
interpretInstruction (IMakeInteger n) = makeInteger n
interpretInstruction inst
  = error
    ( "interpretInstruction: "
      <> show inst
      <> " case is not yet implemented"
    )

makeInteger :: Integer -> InterpreterMonad ()
makeInteger n = do
  addr <- allocNodeOnHeap (NInteger n)
  pushAddrToStack addr
  return ()
