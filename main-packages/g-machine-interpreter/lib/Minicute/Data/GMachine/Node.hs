{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Minicute.Data.GMachine.Node
  ( module Minicute.Data.GMachine.Address
  , module Minicute.Data.GMachine.Instruction

  , Node( .. )
  , isValueNode
  ) where

import Data.Data
import Data.Text.Prettyprint.Doc ( Pretty(..) )
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

instance Pretty Node where
  pretty NEmpty = "empty"
  pretty (NInteger n) = pretty n
  pretty (NStructure tag addr)
    = "$C{" PP.<> pretty tag PP.<> ";" PP.<> pretty addr PP.<> "}"
  pretty (NStructureFields _ addrs)
    = "$F" PP.<+> prettyList addrs
  pretty (NApplication fAddr argAddr)
    = pretty fAddr PP.<+> "$" PP.<+> pretty argAddr
  pretty (NIndirect addr)
    = "~>" PP.<+> pretty addr
  pretty (NGlobal arity insts)
    = "global<" PP.<> pretty arity PP.<> ">" PP.<+> PP.unsafeViaShow insts

isValueNode :: Node -> Bool
isValueNode (NInteger _) = True
isValueNode (NStructure _ _) = True
isValueNode _ = False
