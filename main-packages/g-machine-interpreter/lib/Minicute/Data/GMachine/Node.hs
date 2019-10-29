{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Minicute.Data.GMachine.Node
  ( module Minicute.Data.GMachine.Address
  , module Minicute.Data.GMachine.Instruction

  , Node( .. )
  , isValueNode

  , prettyNode
  ) where

import Data.Data
import GHC.Generics
import Minicute.Data.GMachine.Address
import Minicute.Data.GMachine.Instruction

import qualified Data.Text.Prettyprint.Doc as PP

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

prettyNode :: Node -> PP.Doc ann
prettyNode NEmpty = "empty"
prettyNode (NInteger n) = PP.pretty n
prettyNode (NStructure tag addr)
  = "$C" PP.<> PP.braces (PP.pretty tag PP.<> PP.semi PP.<> prettyAddress addr)
prettyNode (NStructureFields _ addrs)
  = "$F" PP.<+> PP.list (fmap prettyAddress addrs)
prettyNode (NApplication fAddr argAddr)
  = prettyAddress fAddr PP.<+> "$" PP.<+> prettyAddress argAddr
prettyNode (NIndirect addr)
  = "~>" PP.<+> prettyAddress addr
prettyNode (NGlobal arity insts)
  = "global" PP.<> PP.angles (PP.pretty arity) PP.<+> PP.unsafeViaShow insts
