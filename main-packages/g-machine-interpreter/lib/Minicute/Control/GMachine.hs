{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- |
-- Copyright: (c) 2018-present Junyoung Clare Jang
-- License: BSD 3-Clause
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

import Control.Monad.Fail
import Control.Monad.State ( MonadState(..), StateT, execStateT, gets, modify )
import Control.Monad.Trans ( MonadTrans(..) )
import Control.Monad.Writer ( MonadWriter(..), Writer, runWriter )
import Data.Data ( Typeable )
import Data.List.NonEmpty ( NonEmpty(..), (<|) )
import Data.Monoid ( First(..) )
import GHC.Generics ( Generic )
import Minicute.Control.GMachine.Step
import Minicute.Data.GMachine.Instruction

import qualified Data.List.NonEmpty as NonEmpty

type GMachineMonad = GMachineMonadT IO

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

  {-# INLINE pure #-}
  {-# INLINABLE (<*>) #-}

instance (Monad m) => Monad (GMachineMonadT m) where
  (GMachineMonadT a) >>= f
    = GMachineMonadT
      $ (>>= fst . runWriter . runGMachineMonadT . f) <$> a
  {-# INLINABLE (>>=) #-}

instance (MonadFail m) => MonadFail (GMachineMonadT m) where
  fail = GMachineMonadT . pure . fail

  {-# INLINE fail #-}

instance (Monad m) => MonadState (NonEmpty GMachineState) (GMachineMonadT m) where
  get = GMachineMonadT . pure $ get
  put = GMachineMonadT . pure . put
  state = GMachineMonadT . pure . state

  {-# INLINE get #-}
  {-# INLINE put #-}
  {-# INLINE state #-}

instance MonadTrans GMachineMonadT where
  lift = GMachineMonadT . pure . lift

  {-# INLINE lift #-}

execGMachineT :: (MonadFail m) => GMachineMonadT m a -> m (NonEmpty GMachineState)
execGMachineT (GMachineMonadT a)
  | Just st <- maySt = NonEmpty.reverse <$> execStateT b (st :| [])
  | otherwise = fail "execGMachineT: input G-Machine is not initialized"
  where
    (b, First maySt) = runWriter a
{-# INLINABLE execGMachineT #-}


initializeGMachineWith :: (Monad m) => GMachineProgram -> GMachineMonadT m ()
initializeGMachineWith
  = GMachineMonadT
    . fmap pure
    . tell
    . pure
    . buildInitialState
{-# INLINABLE initializeGMachineWith #-}

executeGMachineStep :: (Monad m) => GMachineStepMonadT m () -> GMachineMonadT m ()
executeGMachineStep step = do
  nextSt <- getCurrentState >>= makeNextState
  modify (nextSt <|)
  where
    getCurrentState = gets NonEmpty.head
    makeNextState = lift . execGMachineStepT step

    {-# INLINE getCurrentState #-}
    {-# INLINE makeNextState #-}
{-# INLINABLE executeGMachineStep #-}

checkGMachineFinished :: (Monad m) => GMachineMonadT m Bool
checkGMachineFinished
  = gets (checkTerminalState . NonEmpty.head)
{-# INLINE checkGMachineFinished #-}
