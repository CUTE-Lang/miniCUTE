{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Minicute.Data.GMachine.Node
  ( Node( .. )
  , isValueNode
  ) where

import Data.Data
import GHC.Generics
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
           , Show
           )

isValueNode :: Node -> Bool
isValueNode (NInteger _) = True
isValueNode (NStructure _ _) = True
isValueNode _ = False
