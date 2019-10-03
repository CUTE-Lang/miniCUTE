module Minicute.Interpreter.GMachine
  ( module Minicute.Control.GMachine

  , interpretProgram
  ) where

import Control.Monad.Extra
import Minicute.Control.GMachine
import Minicute.Data.GMachine.Instruction
import Minicute.Interpreter.GMachine.Instruction

interpretProgram :: GMachineProgram -> GMachineMonad ()
interpretProgram program
  = initializeInterpreterWith program >> buildSteps
  where
    buildSteps
      = ifM checkInterpreterFinished
        (addInterpreterStep step >> buildSteps)
        (pure ())
    step = fetchNextInstruction >>= interpretInstruction
