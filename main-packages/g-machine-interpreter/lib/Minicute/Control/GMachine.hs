{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Minicute.Control.GMachine
  ( module Minicute.Control.GMachine.Step

  , GMachineMonadT
  , GMachineMonad
  , execGMachineT

  , initializeGMachineWith
  , executeGMachineStep
  , checkGMachineFinished
  ) where

import Prelude hiding ( fail )

import Control.Monad ( (<=<) )
import Control.Monad.Fail
import Control.Monad.State ( MonadState(..), StateT, gets, modify, execStateT )
import Control.Monad.Trans ( MonadTrans(..) )
import Control.Monad.Writer ( MonadWriter(..), Writer, runWriter )
import Data.Data
import Data.List.NonEmpty ( NonEmpty(..), (<|) )
import Data.Monoid ( First(..) )
import GHC.Generics
import Minicute.Control.GMachine.Step
import Minicute.Data.GMachine.Instruction

import qualified Data.List.NonEmpty as NonEmpty

type GMachineMonad = GMachineMonadT Maybe

newtype GMachineMonadT m a
  = GMachineMonadT
    { runGMachineMonadT :: Writer (First GMachineState) (StateT (NonEmpty GMachineState) m a)
    }
  deriving ( Generic
           , Typeable
           , Functor
           )

instance (Monad m) => Applicative (GMachineMonadT m) where
  pure = GMachineMonadT . pure . pure
  (GMachineMonadT f) <*> (GMachineMonadT a)
    = GMachineMonadT $ fmap (<*>) f <*> a

instance (Monad m) => Monad (GMachineMonadT m) where
  (GMachineMonadT a) >>= f
    = GMachineMonadT
      $ (>>= fst . runWriter . runGMachineMonadT . f) <$> a

instance (MonadFail m) => MonadFail (GMachineMonadT m) where
  fail = GMachineMonadT . pure . fail

instance (Monad m) => MonadState (NonEmpty GMachineState) (GMachineMonadT m) where
  get = GMachineMonadT . pure $ get
  put = GMachineMonadT . pure . put
  state = GMachineMonadT . pure . state

instance MonadTrans GMachineMonadT where
  lift = GMachineMonadT . pure . lift

execGMachineT :: (Monad m) => GMachineMonadT m () -> m (NonEmpty GMachineState)
execGMachineT (GMachineMonadT a)
  | Just st <- maySt = execStateT b (st :| [])
  | otherwise = error "execGMachineT: input G-Machine is not initialized"
  where
    (b, First maySt) = runWriter a


initializeGMachineWith :: (Monad m) => GMachineProgram -> GMachineMonadT m ()
initializeGMachineWith
  = GMachineMonadT
    . ( pure . pure
        <=< tell . First . Just . buildInitialState
      )

executeGMachineStep :: (Monad m) => GMachineStepMonadT m () -> GMachineMonadT m ()
executeGMachineStep step = do
  st <- gets NonEmpty.head
  st' <- lift . execGMachineStepT step $ st
  modify (st' <|)

checkGMachineFinished :: (Monad m) => GMachineMonadT m Bool
checkGMachineFinished = gets (checkTerminalState . NonEmpty.head)
