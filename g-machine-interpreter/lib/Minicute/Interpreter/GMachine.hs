{-# LANGUAGE NamedFieldPuns #-}
module Minicute.Interpreter.GMachine
  ( module Minicute.Control.GMachine

  , interpretProgram
  ) where

import Control.Monad.Extra
import Minicute.Data.GMachine.Instruction
import Minicute.Interpreter.GMachine.Instruction
import Minicute.Control.GMachine

interpretProgram :: GMachineProgram -> GMachineMonad ()
interpretProgram program
  = initializeInterpreterWith program >> buildSteps
  where
    buildSteps
      = ifM checkInterpreterFinished
        (addInterpreterStep step >> buildSteps)
        (return ())
    step = fetchNextInstruction >>= interpretInstruction
