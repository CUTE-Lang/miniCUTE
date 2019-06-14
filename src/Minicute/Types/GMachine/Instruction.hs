{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
module Minicute.Types.GMachine.Instruction
  ( -- * G-Machine Architecture

    -- ** Abstract Structure
    -- $abstractStructure

    -- ** Operational Semantics
    -- $operationalSemantics

    -- * Types
    GMachineProgram
  , GMachineSupercombinator
  , GMachineExpression

    -- ** Instructions
  , Instruction( .. )
  , PrimitiveOperator( .. )
  , MatchTable( .. )
  , MatchEntry( .. )
  ) where

import Data.Data
import GHC.Generics
import Language.Haskell.TH.Syntax
import Minicute.Types.Minicute.Precedence

type GMachineProgram = [GMachineSupercombinator]
-- |
-- [@Identifier@] The identifier for the Supercombinator.
--
-- [@Int@] The arity of the Supercombinator.
--
-- [@GMachineExpression@] The code for the Supercombinator.
type GMachineSupercombinator = (Identifier, Int, GMachineExpression)
type GMachineExpression = [Instruction]

-- $abstractStructure
-- This G-Machine is represented by 5-tuple.
--
-- @(CurrentCode, AddressStack, ValueStack, NodeHeap, GlobalEnvironment)@
--
-- [@CurrentCode@] a list of codes currently executed.
--
-- [@AddressStack@] a stack of G-Machine addresses.
--
-- [@ValueStack@] a stack of primitive values.
--
-- [@NodeHeap@] a heap of G-Machine nodes.
--
-- [@GlobalEnvironment@] a map from identifiers to addresses of corresponding nodes.

-- $operationalSemantics
-- Any unspecified cases mean erratic state.
-- 
-- === Basic Node Creating Operations
-- - __IMakeInteger__
-- 
--     > (IMakeInteger n : codes,        addrs, values, heap,                   global)
--     > ------------------------------------------------------------------------------
--     > (                 codes, addr : addrs, values, heap[addr: NInteger n], global)
-- 
-- - __IMakeConstructor__
-- 
--     > (IMakeConstructor t a : codes,        addrs, values, heap,                         global)
--     > ------------------------------------------------------------------------------------------
--     > (                       codes, addr : addrs, values, heap[addr: NConstructor t a], global)
-- 
-- - __IMakeApplication__
-- 
--     > (IMakeApplication : codes, addr_0 : addr_1 : addrs, values, heap,                                   global)
--     > -----------------------------------------------------------------------------------------------------------
--     > (                   codes,            addr : addrs, values, heap[addr: NApplication addr_1 addr_0], global)
-- 
-- - __IMakeGlobal__
-- 
--     > (IMakeGlobal id : codes,        addrs, values, heap, global[id: addr])
--     > ----------------------------------------------------------------------
--     > (                 codes, addr : addrs, values, heap, global[id: addr])
-- 
-- - __IMakePlaceholders__
-- 
--     > (IMakePlaceholders n : codes,                                  addrs, values, heap,  global)
--     > --------------------------------------------------------------------------------------------
--     > (                      codes, addr_0 : addr_1 : ... : addr_n : addrs, values, heap', global)
--     >
--     > heap' = heap[addr_0: NEmpty, addr_1: NEmpty, ..., addr_n: NEmpty]
-- 
--     Do we need to add @NEmpty@ or just use @NIndirect nullAddr@?
-- 
-- === Address Stack Based Operations
-- - __IPop__
-- 
--     > (IPop n : codes, addr_0 : addr_1 : ... : addr_(n - 1) : addrs, values, heap, global)
--     > ------------------------------------------------------------------------------------
--     > (         codes,                                        addrs, values, heap, global)
-- 
-- - __IDig__
-- 
--     > (IDig n : codes, addr_0 : addr_1 : ... : addr_n : addrs, values, heap, global)
--     > ------------------------------------------------------------------------------
--     > (         codes,                         addr_0 : addrs, values, heap, global)
-- 
-- - __IUpdate__
-- 
--     > (IUpdate n : codes, addr_0 : addr_1 : ... : addr_n : addrs, values, heap,                           global)
--     > -----------------------------------------------------------------------------------------------------------
--     > (            codes,          addr_1 : ... : addr_n : addrs, values, heap[addr_n: NIndirect addr_0], global)
-- 
-- - __ICopyArgument__
-- 
--     > (ICopyArgument n : codes,          addr_0 : addr_1 : ... : addr_n : addrs, values, heap, global)
--     > ------------------------------------------------------------------------------------------------
--     > (                  codes, addr_n : addr_0 : addr_1 : ... : addr_n : addrs, values, heap, global)
-- 
-- === Value Stack Based Operations
-- - __IPushBasicValue__
-- 
--     > (IPushBasicValue v : codes, addrs,     values, heap, global)
--     > ------------------------------------------------------------
--     > (                    codes, addrs, v : values, heap, global)
-- 
-- - __IPushExtractedValue__
-- 
--     > (IPushExtractedValue : codes, addr : addrs,     values, heap[addr: NInteger v], global)
--     > ---------------------------------------------------------------------------------------
--     > (                      codes,        addrs, v : values, heap,                   global)
-- 
--     > (IPushExtractedValue : codes, addr : addrs,     values, heap[addr: NStructure v 0], global)
--     > -------------------------------------------------------------------------------------------
--     > (                      codes,        addrs, v : values, heap,                       global)
-- 
-- - __IWrapAsInteger__
-- 
--     > (IWrapAsInteger : codes,        addrs, v : values, heap,                   global)
--     > ----------------------------------------------------------------------------------
--     > (                 codes, addr : addrs,     values, heap[addr: NInteger v], global)
-- 
-- - __IWrapAsConstructor__
-- 
--     __TODO: Rename this instruction.__
-- 
--     > (IWrapAsConstructor : codes,        addrs, v : values, heap,                       global)
--     > ------------------------------------------------------------------------------------------
--     > (                     codes, addr : addrs,     values, heap[addr: NStructure v 0], global)
-- 
-- - __IUpdateAsInteger__
-- 
--     > (IUpdateAsInteger n : codes, addr_0 : addr_1 : ... : addr_n : addrs, v : values, heap,                     global)
--     > ------------------------------------------------------------------------------------------------------------------
--     > (                     codes, addr_0 : addr_1 : ... : addr_n : addrs,     values, heap[addr_n: NInteger v], global)
-- 
-- - __IUpdateAsConstructor__
-- 
--     __TODO: Rename this instruction.__
-- 
--     > (IUpdateAsConstructor n : codes, addr_0 : addr_1 : ... : addr_n : addrs, v : values, heap,                         global)
--     > --------------------------------------------------------------------------------------------------------------------------
--     > (                         codes, addr_0 : addr_1 : ... : addr_n : addrs,     values, heap[addr_n: NStructure v 0], global)
-- 
-- === Primitive Operations
-- - __IPrimitive__
-- 
--     > (IPrimitive op : codes, addrs, v_0 : v_1 : values, heap, global)
--     > ----------------------------------------------------------------
--     > (                codes, addrs,        v' : values, heap, global)
--     >
--     > v' = v_0 R v1
--     > (when op represents a binary operation R)
-- 
--     > (IPrimitive op : codes, addrs,  v : values, heap, global)
--     > ---------------------------------------------------------
--     > (                codes, addrs, v' : values, heap, global)
--     >
--     > v' = R v
--     > (when op represents a unary operation R)
-- 
-- __TODO: Add rest of operation semantics.__

data Instruction
  -- Basic node creating operations
  = IMakeInteger Integer
  | IMakeConstructor Integer Integer
  | IMakeApplication
  | IMakeGlobal Identifier
  | IMakePlaceholders Int

  -- Address Stack based operations
  | IPop Int
  | IDig Int
  | IUpdate Int
  | ICopyArgument Int

  -- Value stack based operations
  | IPushBasicValue Integer
  | IPushExtractedValue
  | IWrapAsInteger
  | IWrapAsConstructor
  | IUpdateAsInteger Int
  | IUpdateAsConstructor Int

  -- Primitive operations
  | IPrimitive PrimitiveOperator

  -- Node inspecting operations
  | IUnwind
  | IDestruct Integer

  -- Dump related operations
  | IEval
  | IReturn

  -- Match related operations
  | IMatch MatchTable
  deriving ( Generic
           , Typeable
           , Data
           , Lift
           , Eq
           , Ord
           , Show
           )

newtype MatchTable
  = MatchTable [MatchEntry]
  deriving ( Generic
           , Typeable
           , Data
           , Lift
           , Eq
           , Ord
           , Show
           )

newtype MatchEntry
  = MatchEntry (Integer, GMachineExpression)
  deriving ( Generic
           , Typeable
           , Data
           , Lift
           , Eq
           , Ord
           , Show
           )

data PrimitiveOperator
  = POAdd
  | POSub
  | POMul
  | PODiv
  deriving ( Generic
           , Typeable
           , Data
           , Lift
           , Eq
           , Ord
           , Show
           )
