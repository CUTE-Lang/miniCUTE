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

import Control.Lens.TH
import Control.Monad.State
import Data.Data
import GHC.Generics
import Minicute.Data.GMachine.Address
import Minicute.Data.GMachine.Node

newtype Heap
  = Heap [Node]
  deriving ( Generic
           , Typeable
           , Data
           , Eq
           )

makeWrapped ''Heap

emptyHeap :: Heap
emptyHeap = Heap []

allocNode :: (MonadState s m, s ~ Heap) => Node -> m Address
allocNode = undefined
