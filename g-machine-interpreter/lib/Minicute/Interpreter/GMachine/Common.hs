{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Minicute.Interpreter.GMachine.Common
  ( InterpreterNode( .. )

  , InterpreterAddress( .. )
  ) where

import Data.Data
import GHC.Generics
import Language.Haskell.TH.Syntax
import Minicute.Data.GMachine.Instruction

data InterpreterNode
  = NEmpty
  | NInteger Integer
  | NStructure Integer InterpreterAddress
  | NStructureFields Integer [InterpreterAddress]
  | NApplication InterpreterAddress InterpreterAddress
  | NIndirect InterpreterAddress
  | NGlobal Integer GMachineExpression
  deriving ( Generic
           , Typeable
           , Data
           , Lift
           , Eq
           , Ord
           , Show
           )

newtype InterpreterAddress
  = InterpreterAddress Integer
  deriving ( Generic
           , Typeable
           , Data
           , Lift
           , Eq
           , Ord
           , Num
           , Show
           )
