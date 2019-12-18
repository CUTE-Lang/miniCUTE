-- |
-- Copyright: (c) 2018-present Junyoung Clare Jang
-- License: BSD 3-Clause
module Minicute.Interpreter.GMachine
  ( module Minicute.Control.GMachine

  , interpretProgram
  ) where

import Control.Monad.Extra ( ifM )
import Minicute.Control.GMachine
import Minicute.Data.GMachine.Instruction
import Minicute.Interpreter.GMachine.Instruction

interpretProgram :: GMachineProgram -> GMachineMonad ()
interpretProgram program = do
  initializeGMachineWith program
  buildSteps
  where
    buildSteps
      = ifM checkGMachineFinished
        (pure ())
        (executeGMachineStep step >> buildSteps)
    step = fetchNextInstruction >>= interpretInstruction
    {-# INLINE step #-}
{-# INLINABLE interpretProgram #-}
