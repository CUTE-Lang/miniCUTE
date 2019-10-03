{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Minicute.Data.GMachine.NodeHeap
  ( module Minicute.Data.GMachine.Address

  , NodeHeap
  , empty
  , allocNode
  , updateNode
  , findNode
  ) where

import Control.Lens.At ( at )
import Control.Lens.Getter ( use )
import Control.Lens.Operators
import Control.Lens.TH
import Control.Lens.Tuple
import Control.Lens.Wrapped ( _Wrapped )
import Control.Monad.Fail ( MonadFail )
import Control.Monad.State
import Data.Data
import GHC.Generics
import Minicute.Data.GMachine.Address
import Minicute.Data.GMachine.Node

import qualified Data.Map as Map

newtype NodeHeap
  = NodeHeap (Address, Map.Map Address Node)
  deriving ( Generic
           , Typeable
           , Data
           , Eq
           , Ord
           , Show
           )

makeWrapped ''NodeHeap

empty :: NodeHeap
empty = NodeHeap (minimumAddress, Map.empty)

allocNode :: (MonadState s m, s ~ NodeHeap) => Node -> m Address
allocNode node = do
  addr <- _Wrapped . _1 <%= increaseAddress
  _Wrapped . _2 %= Map.insert addr node
  return addr

updateNode :: (MonadState s m, s ~ NodeHeap, MonadFail m) => Address -> Node -> m ()
updateNode addr node = _Wrapped . _2 %= Map.alter alter addr
  where
    alter (Just _) = Just node
    alter Nothing = fail ("updateNode: there is no node for address " <> show addr)

findNode :: (MonadState s m, s ~ NodeHeap, MonadFail m) => Address -> m Node
findNode addr = do
  mayNode <- use (_Wrapped . _2 . at addr)
  case mayNode of
    Just node -> pure node
    Nothing -> fail ("findNode: there is no node for address " <> show addr)
