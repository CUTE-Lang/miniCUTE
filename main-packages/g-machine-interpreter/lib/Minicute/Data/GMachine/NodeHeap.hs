{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
module Minicute.Data.GMachine.NodeHeap
  ( module Minicute.Data.GMachine.Address

  , NodeHeap
  , empty
  , allocNode
  , updateNode
  , findNode

  , prettyNodeHeap
  ) where

import Prelude hiding ( fail )

import Control.Lens.At ( at )
import Control.Lens.Getter ( use )
import Control.Lens.Operators
import Control.Lens.Operators.Minicute
import Control.Lens.TH
import Control.Lens.Tuple
import Control.Lens.Wrapped ( _Wrapped )
import Control.Monad.Fail
import Control.Monad.State ( MonadState )
import Data.Data
import GHC.Generics
import Minicute.Data.GMachine.Address
import Minicute.Data.GMachine.Node

import qualified Data.Map as Map
import qualified Data.Text.Prettyprint.Doc as PP

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
  _Wrapped . _1 %= increaseAddress
  _Wrapped %%= \(addr, m) -> (addr, (addr, Map.insert addr node m))

updateNode :: (MonadState s m, s ~ NodeHeap, MonadFail m) => Address -> Node -> m ()
updateNode addr node = _Wrapped . _2 %%~= fmap ((),) . Map.alterF updateNode' addr
  where
    updateNode' (Just _) = pure (Just node)
    updateNode' Nothing = fail $ "updateNode: there is no node for address " <> show addr

findNode :: (MonadState s m, s ~ NodeHeap, MonadFail m) => Address -> m Node
findNode addr = use (_Wrapped . _2 . at addr) >>= findNode'
  where
    findNode' (Just node) = pure node
    findNode' Nothing = fail $ "findNode: there is no node for address " <> show addr

prettyNodeHeap :: NodeHeap -> PP.Doc ann
prettyNodeHeap (NodeHeap (lastAddr, m))
  = "node" PP.<+> "heap" PP.<+> PP.angles (prettyAddress lastAddr)
    PP.<+>
    PP.braces
    ( if Map.null m
      then
        PP.hardline
      else
        PP.enclose PP.hardline PP.hardline
        . PP.indent 2
        . prettyNodeHeapItems
        $ Map.toAscList m
    )
  where
    prettyNodeHeapItems = PP.vsep . fmap prettyNodeHeapItem
    prettyNodeHeapItem (addr, node)
      = prettyAddress addr PP.<> PP.colon PP.<+> prettyNode node
