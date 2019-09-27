{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
module Minicute.Data.GMachine.Node
  ( GMachineNode( .. )
  , isValueGMachineNode
  ) where

import Data.Data
import GHC.Generics
import Language.Haskell.TH.Syntax
import Minicute.Data.GMachine.Instruction
import Minicute.Data.GMachine.Address

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
           , Lift
           , Eq
           , Ord
           , Show
           )

isValueGMachineNode :: GMachineNode -> Bool
isValueGMachineNode (NInteger _) = True
isValueGMachineNode (NStructure _ _) = True
isValueGMachineNode _ = False
