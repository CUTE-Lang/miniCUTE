{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Minicute.Data.GMachine.Heap
  ( module Minicute.Data.Common
  , module Minicute.Data.GMachine.Address

  , GMachineHeap
  , emptyHeap
  , allocNode
  ) where

import Control.Lens.TH
import Control.Monad.State
import Data.Data
import GHC.Generics
import Minicute.Data.Common
import Minicute.Data.GMachine.Address
import Minicute.Data.GMachine.Node

newtype GMachineHeap
  = GMachineHeap [GMachineNode]
  deriving ( Generic
           , Typeable
           , Data
           , Eq
           )

makeWrapped ''GMachineHeap

emptyHeap :: GMachineHeap
emptyHeap = GMachineHeap []

allocNode :: (MonadState s m, s ~ GMachineHeap) => GMachineNode -> m GMachineAddress
allocNode = undefined
