{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
module Minicute.Interpreter.GMachine
  ( interpretProgram
  ) where

import Control.Monad
import Control.Monad.State
import Data.Data
import Data.Foldable
import Data.Functor.Identity
import GHC.Generics
import Language.Haskell.TH.Syntax
import Minicute.Data.GMachine.Instruction
import Minicute.Interpreter.GMachine.State

interpretProgram :: GMachineProgram -> InterpreterMonad ()
interpretProgram = void . runStateT executeProgram <=< getInitialState
  where
    getInitialState program = do
      stateGlobal <- createGlobal program
      return
        $ InterpreterState
        { stateCode = initialCode
        , stateStack = emptyStack
        , stateHeap = emptyHeap
        , stateGlobal
        }

    createGlobal :: [GMachineSupercombinator] -> InterpreterMonad InterpreterGlobal
    createGlobal = foldrM addSupercombinatorToGlobal emptyGlobal

executeProgram :: StateT InterpreterState InterpreterMonad ()
executeProgram = do
  st <- get
  case getNextInstruction st of
    Just (inst, st') -> do
      put st'
      executeInstruction inst
      executeProgram
    Nothing -> return ()

executeInstruction :: Instruction -> StateT InterpreterState InterpreterMonad ()
executeInstruction (IMakeInteger n) = makeInteger n
executeInstruction _ = error "executeInstruction: Not Yet Implemented"

makeInteger :: Integer -> StateT InterpreterState InterpreterMonad ()
makeInteger _ = do
  _ <- get
  undefined

type InterpreterMonad = InterpreterMonadT Identity

newtype InterpreterMonadT m a = InterpreterM (m a)
  deriving ( Generic
           , Typeable
           , Data
           , Lift
           , Eq
           , Ord
           , Functor
           , Applicative
           , Monad
           )

instance Lift a => Lift (Identity a) where
  lift (Identity a) = [| Identity a |]
