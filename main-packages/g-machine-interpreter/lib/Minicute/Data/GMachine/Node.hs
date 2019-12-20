{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
-- |
-- Copyright: (c) 2018-present Junyoung Clare Jang
-- License: BSD 3-Clause
module Minicute.Data.GMachine.Node
  ( module Minicute.Data.GMachine.Address
  , module Minicute.Data.GMachine.Instruction

  , Node( .. )
  , isValueNode
  ) where

import Data.Data ( Data, Typeable )
import GHC.Generics ( Generic )
import Minicute.Data.GMachine.Address
import Minicute.Data.GMachine.Instruction

data Node
  = NEmpty
  | NInteger Integer
  | NStructure Integer Address
  | NStructureFields Integer [Address]
  | NApplication Address Address
  | NIndirect Address
  | NGlobal Integer GMachineExpression
  deriving ( Generic
           , Typeable
           , Data
           , Eq
           , Ord
           , Show
           )

isValueNode :: Node -> Bool
isValueNode (NInteger _) = True
isValueNode (NStructure _ _) = True
isValueNode _ = False
{-# INLINABLE isValueNode #-}
