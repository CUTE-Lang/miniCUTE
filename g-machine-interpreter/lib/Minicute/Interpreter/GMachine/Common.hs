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
  = NInteger Integer
  | NApplication InterpreterAddress InterpreterAddress
  | NGlobal Integer GMachineExpression
  deriving ( Generic
           , Typeable
           , Data
           , Lift
           , Eq
           , Ord
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
           )
