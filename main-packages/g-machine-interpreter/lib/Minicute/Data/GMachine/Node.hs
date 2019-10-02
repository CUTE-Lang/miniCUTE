{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Minicute.Data.GMachine.Node
  ( GMachineNode( .. )
  , isValueGMachineNode
  ) where

import Data.Data
import GHC.Generics
import Minicute.Data.GMachine.Address
import Minicute.Data.GMachine.Instruction

data GMachineNode
  = NEmpty
  | NInteger Integer
  | NStructure Integer GMachineAddress
  | NStructureFields Integer [GMachineAddress]
  | NApplication GMachineAddress GMachineAddress
  | NIndirect GMachineAddress
  | NGlobal Integer GMachineExpression
  deriving ( Generic
           , Typeable
           , Data
           , Eq
           , Show
           )

isValueGMachineNode :: GMachineNode -> Bool
isValueGMachineNode (NInteger _) = True
isValueGMachineNode (NStructure _ _) = True
isValueGMachineNode _ = False
