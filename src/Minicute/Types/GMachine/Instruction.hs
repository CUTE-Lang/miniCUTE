{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
-- |
-- Instructions and other helper types for G-Machine
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

-- |
-- A G-Machine program.
type GMachineProgram = [GMachineSupercombinator]
-- |
-- A G-Machine top-level definition.
--
-- It contains
--
-- [@Identifier@] The identifier for the Supercombinator.
--
-- [@Int@] The arity of the Supercombinator.
--
-- [@GMachineExpression@] The code for the Supercombinator.
type GMachineSupercombinator = (Identifier, Int, GMachineExpression)
-- |
-- A G-Machine expression
type GMachineExpression = [Instruction]

-- $abstractStructure
-- This G-Machine is represented by 6-tuple.
--
-- @(CurrentCode, AddressStack, ValueStack, NodeHeap, GlobalEnvironment)@
--
-- [@CurrentCode@] a list of codes currently executed.
--
-- [@AddressStack@] a stack of G-Machine addresses.
--
-- [@ValueStack@] a stack of primitive values.
--
-- [@EvalDump@] a stack of snapshots of codes, address stacks, and value stacks.
--
-- [@NodeHeap@] a heap of G-Machine nodes.
--
-- [@GlobalEnvironment@] a map from identifiers to addresses of corresponding nodes.
--

-- $operationalSemantics
-- Any unspecified cases mean erratic state.
--
-- === Basic Node Creating Operations
-- - __IMakeInteger__
--
--     > (IMakeInteger n : codes,        addrs, values, dump, heap,                   global)
--     > ------------------------------------------------------------------------------------
--     > (                 codes, addr : addrs, values, dump, heap[addr: NInteger n], global)
--
-- - __IMakeConstructor__
--
--     > (IMakeConstructor t n : codes,        addrs, values, dump, heap,                         global)
--     > ------------------------------------------------------------------------------------------------
--     > (                       codes, addr : addrs, values, dump, heap[addr: NConstructor t n], global)
--
-- - __IMakeStructure__
--
--     > (IMakeStructure t n : codes, addr_0 : addr_1 : ... : addr_(n - 1) : addrs, values, dump, heap,                                                       global)
--     > ------------------------------------------------------------------------------------------------------------------------------------------------------------
--     > (                     codes,                                 addr : addrs, values, dump, heap[addr: NStructure t [addr_(n-1), ..., addr_1, addr_0]], global)
--
-- - __IMakeApplication__
--
--     > (IMakeApplication : codes, addr_0 : addr_1 : addrs, values, dump, heap,                                   global)
--     > -----------------------------------------------------------------------------------------------------------------
--     > (                   codes,            addr : addrs, values, dump, heap[addr: NApplication addr_1 addr_0], global)
--
-- - __IMakeGlobal__
--
--     > (IMakeGlobal id : codes,        addrs, values, dump, heap, global[id: addr])
--     > ----------------------------------------------------------------------------
--     > (                 codes, addr : addrs, values, dump, heap, global[id: addr])
--
-- - __IMakePlaceholders__
--
--     > (IMakePlaceholders n : codes,                                  addrs, values, dump, heap,  global)
--     > --------------------------------------------------------------------------------------------------
--     > (                      codes, addr_0 : addr_1 : ... : addr_n : addrs, values, dump, heap', global)
--     >
--     > heap' = heap[addr_0: NEmpty, addr_1: NEmpty, ..., addr_n: NEmpty]
--
--     Do we need to add @NEmpty@ or just use @NIndirect nullAddr@?
--
-- === Address Stack Based Operations
-- - __IPop__
--
--     > (IPop n : codes, addr_0 : addr_1 : ... : addr_(n - 1) : addrs, values, dump, heap, global)
--     > ------------------------------------------------------------------------------------------
--     > (         codes,                                        addrs, values, dump, heap, global)
--
-- - __IDig__
--
--     > (IDig n : codes, addr_0 : addr_1 : ... : addr_n : addrs, values, dump, heap, global)
--     > ------------------------------------------------------------------------------------
--     > (         codes,                         addr_0 : addrs, values, dump, heap, global)
--
-- - __IUpdate__
--
--     > (IUpdate n : codes, addr_0 : addr_1 : ... : addr_n : addrs, values, dump, heap,                           global)
--     > -----------------------------------------------------------------------------------------------------------------
--     > (            codes,          addr_1 : ... : addr_n : addrs, values, dump, heap[addr_n: NIndirect addr_0], global)
--
-- - __ICopy__
--
--     > (ICopy n : codes,          addr_0 : addr_1 : ... : addr_n : addrs, values, dump, heap, global)
--     > ----------------------------------------------------------------------------------------------
--     > (          codes, addr_n : addr_0 : addr_1 : ... : addr_n : addrs, values, dump, heap, global)
--
-- === Value Stack Based Operations
-- - __IPushBasicValue__
--
--     > (IPushBasicValue v : codes, addrs,     values, dump, heap, global)
--     > ------------------------------------------------------------------
--     > (                    codes, addrs, v : values, dump, heap, global)
--
-- - __IPushExtractedValue__
--
--     > (IPushExtractedValue : codes, addr : addrs,     values, dump, heap[addr: NInteger v], global)
--     > ---------------------------------------------------------------------------------------------
--     > (                      codes,        addrs, v : values, dump, heap,                   global)
--
--     > (IPushExtractedValue : codes, addr : addrs,     values, dump, heap[addr: NStructure v []], global)
--     > --------------------------------------------------------------------------------------------------
--     > (                      codes,        addrs, v : values, dump, heap,                        global)
--
-- - __IWrapAsInteger__
--
--     > (IWrapAsInteger : codes,        addrs, v : values, dump, heap,                   global)
--     > ----------------------------------------------------------------------------------------
--     > (                 codes, addr : addrs,     values, dump, heap[addr: NInteger v], global)
--
-- - __IWrapAsStructure__
--
--     > (IWrapAsStructure : codes,        addrs, v : values, dump, heap,                        global)
--     > -----------------------------------------------------------------------------------------------
--     > (                   codes, addr : addrs,     values, dump, heap[addr: NStructure v []], global)
--
-- - __IUpdateAsInteger__
--
--     > (IUpdateAsInteger n : codes, addr_0 : addr_1 : ... : addr_n : addrs, v : values, dump, heap,                     global)
--     > ------------------------------------------------------------------------------------------------------------------------
--     > (                     codes, addr_0 : addr_1 : ... : addr_n : addrs,     values, dump, heap[addr_n: NInteger v], global)
--
-- - __IUpdateAsStructure__
--
--     > (IUpdateAsStructure n : codes, addr_0 : addr_1 : ... : addr_n : addrs, v : values, dump, heap,                          global)
--     > -------------------------------------------------------------------------------------------------------------------------------
--     > (                       codes, addr_0 : addr_1 : ... : addr_n : addrs,     values, dump, heap[addr_n: NStructure v []], global)
--
-- === Primitive Operations
-- - __IPrimitive__
--
--     > (IPrimitive op : codes, addrs, v_0 : v_1 : values, dump, heap, global)
--     > ----------------------------------------------------------------------
--     > (                codes, addrs,        v' : values, dump, heap, global)
--     >
--     > v' = v_0 R v1
--     > (when op represents a binary operation R)
--
--     > (IPrimitive op : codes, addrs,  v : values, dump, heap, global)
--     > ---------------------------------------------------------------
--     > (                codes, addrs, v' : values, dump, heap, global)
--     >
--     > v' = R v
--     > (when op represents a unary operation R)
--
-- === Node Inspecting Operations
-- - __IUnwind__
--
--     > ([IUnwind], addr : addrs, values, dump, heap[addr: NInteger n], global)
--     > -----------------------------------------------------------------------
--     > /Not Yet Possible/
--
--     > ([IUnwind], addr : addrs, values, dump, heap[addr: NStructure t fAddrs], global)
--     > --------------------------------------------------------------------------------
--     > /Not Yet Possible/
--
--     > ([IUnwind],          addr : addrs, values, dump, heap[addr: NApplication addr_1 addr_0], global)
--     > ------------------------------------------------------------------------------------------------
--     > ([IUnwind], addr_1 : addr : addrs, values, dump, heap,                                   global)
--
--     > ([IUnwind],  addr : addrs, values, dump, heap[addr: NIndirect addr'], global)
--     > -----------------------------------------------------------------------------
--     > ([IUnwind], addr' : addrs, values, dump, heap,                        global)
--
--     > (                             [IUnwind],    addr_0 : addr_1 : ... : addr_n : addrs, values, dump, heap[addr_0: NConstructor t n], global)
--     > -----------------------------------------------------------------------------------------------------------------------------------------
--     > (IRearrange (n - 1) : constructorCode t,             addr_1 : ... : addr_n : addrs, values, dump, heap,                           global)
--
--     > ([IUnwind], addr_0 : addr_1 : ... : addr_m : addrs, values, dump, heap[addr_0: NConstructor t n], global)
--     > ---------------------------------------------------------------------------------------------------------
--     > /Not Yet Possible/
--     >
--     > (when m < n)
--
--     > ([IUnwind],  addr_0 : addr_1 : ... : addr_n : addrs, values, dump, heap[addr_0: NGlobal n codes'], global)
--     > ----------------------------------------------------------------------------------------------------------
--     > (   codes',           addr_1 : ... : addr_n : addrs, values, dump, heap,                           global)
--
--     > ([IUnwind], addr_0 : addr_1 : ... : addr_m : addrs, values, dump, heap[addr_0: NGlobal n codes], global)
--     > --------------------------------------------------------------------------------------------------------
--     > /Not Yet Possible/
--     >
--     > (when m < n)
--
--     > (                 [IUnwind],    addr_0 : addr_1 : ... : addr_n : addrs, values, dump, heap[addr_0: NGlobal n codes], global)
--     > ----------------------------------------------------------------------------------------------------------------------------
--     > (IRearrange (n - 1) : codes,             addr_1 : ... : addr_n : addrs, values, dump, heap,                          global)
--
-- - __IDestruct__
--
--     > (IDestruct n : codes,                               addr : addrs, values, dump, heap[addr: NStructure t [addr_(n-1), ..., addr_1, addr_0]], global)
--     > ---------------------------------------------------------------------------------------------------------------------------------------------------
--     > (              codes, addr_0 : addr_1 : ... : addr_(n-1) : addrs, values, dump, heap,                                                       global)
--
-- === Virtual Operations
-- Following operations are not real constructors of 'Instruction'.
-- These exist only for semantic purposes.
--
-- - __IRearrange__
--
--     > (IRearrange n : codes,             addr_0 : addr_1 : ... : addr_n : addrs, values, dump, heap[addr_/i/: NApplication addr_/i/'' addr_/i/'], global)
--     > ---------------------------------------------------------------------------------------------------------------------------------------------------
--     > (               codes, addr_0' : addr_1' : ... : addr_n' : addr_n : addrs, values, dump, heap',                                             global)
--
--
-- __TODO: Add rest of operation semantics.__

-- |
-- A G-Machine instruction
data Instruction
  -- Basic node creating operations
  = IMakeInteger Integer
  | IMakeConstructor Integer Integer
  | IMakeStructure Integer Integer
  | IMakeApplication
  | IMakeGlobal Identifier
  | IMakePlaceholders Int

  -- Address Stack based operations
  | IPop Int
  | IDig Int
  | IUpdate Int
  | ICopy Int

  -- Value stack based operations
  | IPushBasicValue Integer
  | IPushExtractedValue
  | IWrapAsInteger
  | IWrapAsStructure
  | IUpdateAsInteger Int
  | IUpdateAsStructure Int

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

-- |
-- Table expressing a match expression.
--
-- [@[MatchEntry\]@] cases in the match expression.
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

-- |
-- Entry expressing a @match@ case.
--
-- [@Integer@] the tag of the case.
--
-- [@GMachineExpression@] the instructions of the case.
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

-- |
-- Primitive operations done in the value stack.
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
