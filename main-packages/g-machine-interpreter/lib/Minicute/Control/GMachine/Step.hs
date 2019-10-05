{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
module Minicute.Control.GMachine.Step
  ( module Minicute.Data.GMachine.State

  , GMachineStepMonadT
  , GMachineStepMonad
  , runGMachineStepT
  , execGMachineStepT
  ) where

import Prelude hiding ( fail )

import Control.Monad.Fail
import Control.Monad.State ( MonadState(..), StateT, runStateT )
import Control.Monad.Trans ( MonadTrans(..) )
import Data.Data
import GHC.Generics
import Minicute.Data.GMachine.State

type GMachineStepMonad = GMachineStepMonadT Maybe

newtype GMachineStepMonadT m a = GMachineStepMonadT (StateT GMachineState m a)
  deriving ( Generic
           , Typeable
           , Functor
           , Applicative
           , Monad
           , MonadFail
           , MonadTrans
           )

deriving instance (Monad m) => MonadState GMachineState (GMachineStepMonadT m)

runGMachineStepT :: GMachineStepMonadT m a -> GMachineState -> m (a, GMachineState)
runGMachineStepT (GMachineStepMonadT st) = runStateT st

execGMachineStepT :: (Monad m) => GMachineStepMonadT m a -> GMachineState -> m GMachineState
execGMachineStepT = (fmap snd .) . runGMachineStepT
