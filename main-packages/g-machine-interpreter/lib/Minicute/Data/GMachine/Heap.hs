{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Minicute.Data.GMachine.Heap
  ( module Minicute.Data.GMachine.Address

  , Heap
  , emptyHeap
  , allocNode
  ) where

import Control.Lens.Operators
import Control.Lens.TH
import Control.Lens.Tuple
import Control.Lens.Wrapped ( _Wrapped )
import Control.Monad.State
import Data.Data
import GHC.Generics
import Minicute.Data.GMachine.Address
import Minicute.Data.GMachine.Node

import qualified Data.Map as Map

newtype Heap
  = Heap (Address, Map.Map Address Node)
  deriving ( Generic
           , Typeable
           , Data
           , Eq
           , Ord
           )

makeWrapped ''Heap

emptyHeap :: Heap
emptyHeap = Heap (minimumAddress, Map.empty)

allocNode :: (MonadState s m, s ~ Heap) => Node -> m Address
allocNode node = do
  addr <- _Wrapped . _1 <%= increaseAddress
  _Wrapped . _2 %= Map.insert addr node
  return addr
