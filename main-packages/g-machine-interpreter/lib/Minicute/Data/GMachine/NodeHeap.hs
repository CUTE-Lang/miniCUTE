{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Copyright: (c) 2018-present Junyoung Clare Jang
-- License: BSD 3-Clause
module Minicute.Data.GMachine.NodeHeap
  ( module Minicute.Data.GMachine.Address

  , NodeHeap
  , empty
  , allocNode
  , updateNode
  , findNode

  , mark
  , sweep
  ) where

import Prelude hiding ( fail )

import Control.Lens.At ( at )
import Control.Lens.Getter ( use )
import Control.Lens.Operators
import Control.Lens.Operators.Minicute
import Control.Lens.TH
import Control.Lens.Tuple
import Control.Lens.Wrapped ( _Wrapped )
import Control.Monad ( unless )
import Control.Monad.Fail
import Control.Monad.State ( MonadState )
import Data.Data
import Data.Foldable ( traverse_ )
import GHC.Generics
import Minicute.Data.GMachine.Address
import Minicute.Data.GMachine.Node

import qualified Data.Map as Map

newtype NodeHeap
  = NodeHeap (Address, Map.Map Address (Bool, Node))
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
  _Wrapped . _1 %= increaseAddress
  _Wrapped %%= \(addr, m) -> (addr, (addr, Map.insert addr (False, node) m))

updateNode :: (MonadState s m, s ~ NodeHeap, MonadFail m) => Address -> Node -> m ()
updateNode addr node = _Wrapped . _2 %%~= fmap ((),) . Map.alterF updateNode' addr
  where
    updateNode' (Just _) = pure (Just (False, node))
    updateNode' Nothing = fail $ "updateNode: there is no node for address " <> show addr

findNode :: (MonadState s m, s ~ NodeHeap, MonadFail m) => Address -> m Node
findNode addr = use (_Wrapped . _2 . at addr) >>= findNode'
  where
    findNode' (Just (_, node)) = pure node
    findNode' Nothing = fail $ "findNode: there is no node for address " <> show addr

mark :: (MonadState s m, s ~ NodeHeap, MonadFail m) => [Address] -> m ()
mark = foldl ((. markFromAddress) . (>>)) (pure ())
  where
    markFromAddress :: (MonadState s m, s ~ NodeHeap, MonadFail m) => Address -> m ()
    markFromAddress addr = do
      (premarked, node) <- markNode addr
      unless premarked $
        case node of
          NStructure _ fAddr -> markFromAddress fAddr
          NStructureFields _ fAddrs -> traverse_ markFromAddress fAddrs
          NApplication funAddr argAddr -> do
            markFromAddress funAddr
            markFromAddress argAddr
          NIndirect addr' -> markFromAddress addr'
          NEmpty -> pure ()
          NInteger _ -> pure ()
          NGlobal _ _ -> pure ()

    markNode :: (MonadState s m, s ~ NodeHeap, MonadFail m) => Address -> m (Bool, Node)
    markNode addr = do
      mayEntry <- _Wrapped . _2 . at addr <<%= fmap (_1 .~ True)
      case mayEntry of
        Just entry -> pure entry
        Nothing -> fail $ "markNode: there is no node for address " <> show addr

sweep :: (MonadState s m, s ~ NodeHeap) => m ()
sweep = _Wrapped . _2 %= Map.mapMaybe f
  where
    f (True, node) = Just (False, node)
    f (False, _) = Nothing
