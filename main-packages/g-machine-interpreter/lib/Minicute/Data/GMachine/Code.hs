{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Copyright: (c) 2018-present Junyoung Clare Jang
-- License: BSD 3-Clause
module Minicute.Data.GMachine.Code
  ( module Minicute.Data.GMachine.Instruction

  , Code
  , empty
  , initialCode
  , fetchNextInstruction
  , putInstruction
  , putInstructions
  , assertLastCode
  ) where

import Prelude hiding ( fail )

import Control.Lens.Getter ( to, use )
import Control.Lens.Operators
import Control.Lens.Operators.Minicute
import Control.Lens.TH
import Control.Lens.Wrapped ( _Wrapped )
import Control.Monad.Extra ( unlessM )
import Control.Monad.Fail
import Control.Monad.State ( MonadState )
import Data.Data ( Data, Typeable )
import GHC.Generics ( Generic )
import Minicute.Data.GMachine.Instruction

import qualified Minicute.Transpilers.GMachine as GMachine ( initialCode )

newtype Code
  = Code [Instruction]
  deriving ( Generic
           , Typeable
           , Data
           , Eq
           , Ord
           , Show
           )

makeWrapped ''Code

empty :: Code
empty = Code []
{-# INLINE empty #-}

initialCode :: Code
initialCode = Code GMachine.initialCode
{-# INLINE initialCode #-}

fetchNextInstruction :: (MonadState s m, s ~ Code, MonadFail m) => m Instruction
fetchNextInstruction = _Wrapped %%~= fetchNextInstruction'
  where
    fetchNextInstruction' (inst : insts) = pure (inst, insts)
    fetchNextInstruction' _ = fail "popInstructionFromCode: No more instructions exist"
    {-# INLINABLE fetchNextInstruction' #-}
{-# INLINABLE fetchNextInstruction #-}

putInstruction :: (MonadState s m, s ~ Code) => Instruction -> m ()
putInstruction inst = _Wrapped .= [inst]
{-# INLINABLE putInstruction #-}

putInstructions :: (MonadState s m, s ~ Code) => [Instruction] -> m ()
putInstructions insts = _Wrapped .= insts
{-# INLINABLE putInstructions #-}

assertLastCode :: (MonadState s m, s ~ Code, MonadFail m) => m ()
assertLastCode
  = unlessM (use $ _Wrapped . to null)
    $ fail "assertLastCode: Not a last code"
{-# INLINABLE assertLastCode #-}
