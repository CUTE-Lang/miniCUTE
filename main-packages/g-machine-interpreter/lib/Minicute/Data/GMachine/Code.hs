{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Minicute.Data.GMachine.Code
  ( module Minicute.Data.Common
  , module Minicute.Data.GMachine.Instruction

  , Code
  , initialCode
  , popInstructionFromCode
  ) where

import Control.Lens.Getter ( use )
import Control.Lens.Operators
import Control.Lens.TH
import Control.Lens.Wrapped ( _Wrapped )
import Control.Monad.Fail ( MonadFail )
import Control.Monad.State
import Data.Data
import Data.List
import GHC.Generics
import Minicute.Data.Common
import Minicute.Data.GMachine.Instruction

import qualified Minicute.Transpilers.GMachine as GMachine ( initialCode )

newtype Code
  = Code [Instruction]
  deriving ( Generic
           , Typeable
           , Data
           , Eq
           )

makeWrapped ''Code

initialCode :: Code
initialCode = Code GMachine.initialCode

popInstructionFromCode :: (MonadState s m, s ~ Code, MonadFail m) => m Instruction
popInstructionFromCode = do
  instrs <- use _Wrapped
  case uncons instrs of
    Just (instr, instrs') -> do
      _Wrapped .= instrs'
      return instr
    Nothing ->
      fail "no more instruction"
