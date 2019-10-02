{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Minicute.Data.GMachine.State
  ( GMachineState( .. )
  , _code
  , _stack
  , _heap
  , _global
  , getNextInstruction
  ) where

import Prelude hiding ( fail )

import Control.Lens.Getter ( use )
import Control.Lens.Operators
import Control.Lens.TH
import Control.Monad.Fail ( MonadFail )
import Control.Monad.State
import Data.Data
import GHC.Generics

import qualified Minicute.Data.GMachine.Code as Code
import qualified Minicute.Data.GMachine.Global as Global
import qualified Minicute.Data.GMachine.Heap as Heap
import qualified Minicute.Data.GMachine.Stack as Stack

data GMachineState
  = GMachineState
    { code :: Code.GMachineCode
    , stack :: Stack.GMachineStack
    , heap :: Heap.GMachineHeap
    , global ::Global.GMachineGlobal
    }
  deriving ( Generic
           , Typeable
           , Data
           , Eq
           )

makeLensesFor
  [ ("code", "_code")
  , ("stack", "_stack")
  , ("heap", "_heap")
  , ("global", "_global")
  ]
  ''GMachineState

getNextInstruction :: (MonadState s m, s ~ GMachineState, MonadFail m) => m Code.Instruction
getNextInstruction = do
  c <- use _code
  (instr, c') <- runStateT Code.popInstructionFromCode c
  _code .= c'
  return instr
