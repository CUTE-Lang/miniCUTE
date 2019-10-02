{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
module Minicute.Control.GMachine.Step
  ( module Minicute.Data.GMachine.State

  , GMachineStepMonadT
  , GMachineStepMonad
  ) where


import Control.Monad.Fail
import Control.Monad.State
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
           )

deriving instance (Monad m) => MonadState GMachineState (GMachineStepMonadT m)
