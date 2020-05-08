{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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
  , getAllInstructions
  , putInstruction
  , putInstructions
  , assertLastCode
  ) where

import Control.Lens.Getter ( to, use )
import Control.Lens.Iso ( Iso', coerced )
import Control.Lens.Operators
import Control.Lens.Operators.Minicute
import Control.Monad.Extra ( unlessM )
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

_code :: Iso' Code [Instruction]
_code = coerced
{-# INLINE _code #-}


empty :: Code
empty = Code []
{-# INLINE empty #-}

initialCode :: Code
initialCode = Code GMachine.initialCode
{-# INLINE initialCode #-}

fetchNextInstruction :: (MonadState s m, s ~ Code, MonadFail m) => m Instruction
fetchNextInstruction = _code %%~= fetchNextInstruction'
  where
    fetchNextInstruction' (inst : insts) = pure (inst, insts)
    fetchNextInstruction' _ = fail "popInstructionFromCode: No more instructions exist"
    {-# INLINE fetchNextInstruction' #-}
{-# INLINE fetchNextInstruction #-}

getAllInstructions :: (MonadState s m, s ~ Code) => m [Instruction]
getAllInstructions = use _code
{-# INLINE getAllInstructions #-}

putInstruction :: (MonadState s m, s ~ Code) => Instruction -> m ()
putInstruction inst = _code .= [inst]
{-# INLINE putInstruction #-}

putInstructions :: (MonadState s m, s ~ Code) => [Instruction] -> m ()
putInstructions insts = _code .= insts
{-# INLINE putInstructions #-}

assertLastCode :: (MonadState s m, s ~ Code, MonadFail m) => m ()
assertLastCode
  = unlessM (use $ _code . to null)
    $ fail "assertLastCode: Not a last code"
{-# INLINE assertLastCode #-}
