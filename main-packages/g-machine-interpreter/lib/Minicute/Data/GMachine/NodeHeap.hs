{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

  , getLastAddress
  , getNodeHeapMap

  , mark
  , sweep
  ) where

import Control.Lens.At ( at )
import Control.Lens.Getter ( use )
import Control.Lens.Iso ( Iso', coerced )
import Control.Lens.Operators
import Control.Lens.Operators.Minicute
import Control.Lens.Tuple
import Control.Monad ( unless )
import Control.Monad.State ( MonadState )
import Data.Data ( Data, Typeable )
import GHC.Generics ( Generic )
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

_nodeHeap :: Iso' NodeHeap (Address, Map.Map Address (Bool, Node))
_nodeHeap = coerced
{-# INLINE _nodeHeap #-}


empty :: NodeHeap
empty = NodeHeap (minimumAddress, Map.empty)
{-# INLINE empty #-}

allocNode :: (MonadState s m, s ~ NodeHeap) => Node -> m Address
allocNode node
  = _nodeHeap %%= (\(addr, m) -> (addr, (addr, Map.insert addr (False, node) m))) . (_1 %~ increaseAddress)
{-# INLINE allocNode #-}

updateNode :: (MonadState s m, s ~ NodeHeap, MonadFail m) => Address -> Node -> m ()
updateNode addr node = _nodeHeap . _2 . at addr %%~= updateNode'
  where
    updateNode' (Just _) = pure ((), Just (False, node))
    updateNode' Nothing = fail $ "updateNode: there is no node for address " <> show addr
    {-# INLINE updateNode' #-}
{-# INLINE updateNode #-}

findNode :: (MonadState s m, s ~ NodeHeap, MonadFail m) => Address -> m Node
findNode addr = use (_nodeHeap . _2 . at addr) >>= findNode'
  where
    findNode' (Just (_, node)) = pure node
    findNode' Nothing = fail $ "findNode: there is no node for address " <> show addr
    {-# INLINE findNode' #-}
{-# INLINE findNode #-}


getLastAddress :: (MonadState s m, s ~ NodeHeap) => m Address
getLastAddress = use (_nodeHeap . _1)
{-# INLINE getLastAddress #-}

getNodeHeapMap :: (MonadState s m, s ~ NodeHeap) => m (Map.Map Address (Bool, Node))
getNodeHeapMap = use (_nodeHeap . _2)
{-# INLINE getNodeHeapMap #-}


mark :: (MonadState s m, s ~ NodeHeap, MonadFail m) => [Address] -> m ()
mark = mapM_ markNodeByAddress
  where
    markNodeByAddress :: (MonadState s m, s ~ NodeHeap, MonadFail m) => Address -> m ()
    markNodeByAddress addr = do
      (premarked, node) <- markNode addr
      unless premarked $
        case node of
          NStructure _ fAddr -> markNodeByAddress fAddr
          NStructureFields _ fAddrs -> mapM_ markNodeByAddress fAddrs
          NApplication funAddr argAddr -> do
            markNodeByAddress funAddr
            markNodeByAddress argAddr
          NIndirect addr' -> markNodeByAddress addr'
          NEmpty -> pure ()
          NInteger _ -> pure ()
          NGlobal _ _ -> pure ()

    markNode :: (MonadState s m, s ~ NodeHeap, MonadFail m) => Address -> m (Bool, Node)
    markNode addr = do
      mayEntry <- _nodeHeap . _2 . at addr <<%= fmap (_1 .~ True)
      case mayEntry of
        Just entry -> pure entry
        Nothing -> fail $ "markNode: there is no node for address " <> show addr

    {-# INLINE markNode #-}
{-# INLINE mark #-}

sweep :: (MonadState s m, s ~ NodeHeap) => m ()
sweep = _nodeHeap . _2 %= Map.mapMaybe f
  where
    f (True, node) = Just (False, node)
    f (False, _) = Nothing
{-# INLINE sweep #-}
