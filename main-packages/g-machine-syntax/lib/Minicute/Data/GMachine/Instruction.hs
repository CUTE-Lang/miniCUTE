{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
-- |
-- Instructions and other helper types for G-Machine
module Minicute.Data.GMachine.Instruction
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
import Minicute.Data.Precedence

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
-- \((CurrentCode,\ AddressStack,\ ValueStack,\ NodeHeap,\ GlobalEnvironment)\)
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
-- __TODO: make this G-Machine be parallel__

-- $operationalSemantics
-- Any unspecified cases mean erratic state.
--
-- === Basic Node Creating Operations
-- - __IMakeInteger__
--
--     \[
--     \begin{array}{r r r r r l l l}
--       ( & IMakeInteger\ n : codes, &        addrs, & values, & dump, & heap, &                    global & )\\
--       \hline
--       ( &                   codes, & addr : addrs, & values, & dump, & heap[addr: NInteger\ n], & global & )
--     \end{array}
--     \]
--
-- - __IMakeConstructor__
--
--
--     \[
--     \begin{array}{r r r r r l l l}
--       ( & IMakeConstructor\ t\ n : codes, &        addrs, & values, & dump, & heap, &                           global & )\\
--       \hline
--       ( &                          codes, & addr : addrs, & values, & dump, & heap[addr: NConstructor\ t\ n], & global & )
--     \end{array}
--     \]
--
-- - __IMakeStructure__
--
--     \[
--     \begin{array}{r r r r r l l l}
--       ( & IMakeStructure\ t\ n : codes, & addr_0 : addr_1 : ... : addr_{n - 1} : addrs, & values, & dump, & heap, &                                                           global & )\\
--       \hline
--       ( &                        codes, &                                 addr : addrs, & values, & dump, & heap[addr: NStructure\ t\ [addr_{n - 1}, ..., addr_1, addr_0]], & global & )
--     \end{array}
--     \]
--
-- - __IMakeApplication__
--
--     \[
--     \begin{array}{r r r r r l l l}
--       ( & IMakeApplication : codes, & addr_0 : addr_1 : addrs, & values, & dump, & heap, &                                     global & )\\
--       \hline
--       ( &                    codes, &            addr : addrs, & values, & dump, & heap[addr: NApplication\ addr_1\ addr_0], & global & )
--     \end{array}
--     \]
--
-- - __IMakeGlobal__
--
--     \[
--     \begin{array}{r r r r r l l l}
--       ( & IMakeGlobal\ id : codes, &        addrs, & values, & dump, & heap, & global[id: addr] & )\\
--       \hline
--       ( &                   codes, & addr : addrs, & values, & dump, & heap, & global[id: addr] & )
--     \end{array}
--     \]
--
-- - __IMakePlaceholders__
--
--     \[
--     \begin{align}
--     & \begin{array}{r r r r r l l l}
--       ( & IMakePlaceholders\ n : codes, &                                  addrs, & values, & dump, & heap, &  global & )\\
--       \hline
--       ( &                        codes, & addr_0 : addr_1 : ... : addr_n : addrs, & values, & dump, & heap', & global & )
--     \end{array}\\[1em]
--     & heap' = heap[addr_0: NEmpty, addr_1: NEmpty, ..., addr_n: NEmpty]
--     \end{align}
--     \]
--
--     Do we need to add @NEmpty@ or just use @NIndirect nullAddr@?
--
-- === Address Stack Based Operations
-- - __IPop__
--
--     \[
--     \begin{array}{r r r r r l l l}
--       ( & IPop\ n : codes, & addr_0 : addr_1 : ... : addr_{n - 1} : addrs, & values, & dump, & heap, & global & )\\
--       \hline
--       ( &           codes, &                                        addrs, & values, & dump, & heap, & global & )
--     \end{array}
--     \]
--
-- - __IDig__
--
--     \[
--     \begin{array}{r r r r r l l l}
--       ( & IDig\ n : codes, & addr_0 : addr_1 : ... : addr_n : addrs, & values, & dump, & heap, & global & )\\
--       \hline
--       ( &           codes, &                         addr_0 : addrs, & values, & dump, & heap, & global & )
--     \end{array}
--     \]
--
-- - __IUpdate__
--
--     \[
--     \begin{array}{r r r r r l l l}
--       ( & IUpdate\ n : codes, & addr_0 : addr_1 : ... : addr_n : addrs, & values, & dump, & heap, &                            global & )\\
--       \hline
--       ( &              codes, & addr_0 : addr_1 : ... : addr_n : addrs, & values, & dump, & heap[addr_n: NIndirect\ addr_0], & global & )
--     \end{array}
--     \]
--
-- - __ICopy__
--
--     \[
--     \begin{array}{r r r r r l l l}
--       ( & ICopy\ n : codes, &          addr_0 : addr_1 : ... : addr_n : addrs, & values, & dump, & heap, & global & )\\
--       \hline
--       ( &            codes, & addr_n : addr_0 : addr_1 : ... : addr_n : addrs, & values, & dump, & heap, & global & )
--     \end{array}
--     \]
--
-- === Value Stack Based Operations
-- - __IPushBasicValue__
--
--     \[
--     \begin{array}{r r r r r l l l}
--       ( & IPushBasicValue\ v : codes, & addrs, &     values, & dump, & heap, & global & )\\
--       \hline
--       ( &                      codes, & addrs, & v : values, & dump, & heap, & global & )
--     \end{array}
--     \]
--
-- - __IPushExtractedValue__
--
--     \[
--     \begin{array}{r r r r r l l l}
--       ( & IPushExtractedValue : codes, & addr : addrs, &     values, & dump, & heap[addr: NInteger\ v], & global & )\\
--       \hline
--       ( &                       codes, &        addrs, & v : values, & dump, & heap, &                    global & )
--     \end{array}
--     \]
--
--     \[
--     \begin{array}{r r r r r l l l}
--       ( & IPushExtractedValue : codes, & addr : addrs, &     values, & dump, & heap[addr: NStructure\ v\ []], & global & )\\
--       \hline
--       ( &                       codes, &        addrs, & v : values, & dump, & heap, &                          global & )
--     \end{array}
--     \]
--
-- - __IWrapAsInteger__
--
--     \[
--     \begin{array}{r r r r r l l l}
--       ( & IWrapAsInteger : codes, &        addrs, & v : values, & dump, & heap, &                    global & )\\
--       \hline
--       ( &                  codes, & addr : addrs, &     values, & dump, & heap[addr: NInteger\ v], & global & )
--     \end{array}
--     \]
--
-- - __IWrapAsStructure__
--
--     \[
--     \begin{array}{r r r r r l l l}
--       ( & IWrapAsStructure : codes, &        addrs, & v : values, & dump, & heap, &                          global & )\\
--       \hline
--       ( &                    codes, & addr : addrs, &     values, & dump, & heap[addr: NStructure\ v\ []], & global & )
--     \end{array}
--     \]
--
-- - __IUpdateAsInteger__
--
--     \[
--     \begin{array}{r r r r r l l l}
--       ( & IUpdateAsInteger\ n : codes, & addr_0 : addr_1 : ... : addr_n : addrs, & v : values, & dump, & heap, &                      global & )\\
--       \hline
--       ( &                       codes, & addr_0 : addr_1 : ... : addr_n : addrs, &     values, & dump, & heap[addr_n: NInteger\ v], & global & )
--     \end{array}
--     \]
--
-- - __IUpdateAsStructure__
--
--     \[
--     \begin{array}{r r r r r l l l}
--       ( & IUpdateAsStructure\ n : codes, & addr_0 : addr_1 : ... : addr_n : addrs, & v : values, & dump, & heap, &                            global & )\\
--       \hline
--       ( &                         codes, & addr_0 : addr_1 : ... : addr_n : addrs, &     values, & dump, & heap[addr_n: NStructure\ v\ []], & global & )
--     \end{array}
--     \]
--
-- === Primitive Operations
-- - __IPrimitive__
--
--     \[
--     \begin{align}
--     & \begin{array}{r r r r r l l l}
--       ( & IPrimitive\ op : codes, & addrs, & v_0 : v_1 : values, & dump, & heap, & global & )\\
--       \hline
--       ( &                  codes, & addrs, &        v' : values, & dump, & heap, & global & )
--     \end{array}\\
--     & v' = v_0\ R\ v1\\
--     & \text{(when $op$ represents a binary operation $R$)}
--     \end{align}
--     \]
--
--     \[
--     \begin{align}
--     & \begin{array}{r r r r r l l l}
--       ( & IPrimitive\ op : codes, & addrs, &  v : values, & dump, & heap, & global & )\\
--       \hline
--       ( &                  codes, & addrs, & v' : values, & dump, & heap, & global & )
--     \end{array}\\
--     & v' = R\ v\\
--     & \text{(when $op$ represents a unary operation $R$)}
--     \end{align}
--     \]
--
-- === Node Inspecting Operations
-- - __IUnwind__
--
--     \[
--     \begin{array}{r r r r r l l l}
--       ( & [IUnwind], &  addr : addrs, &  values, & (codes', addrs', values') : dump, & heap[addr: NInteger\ n], & global & )\\
--       \hline
--       ( &    codes', & addr : addrs', & values', &                             dump, & heap, &                    global & )
--     \end{array}
--     \]
--
--     \[
--     \begin{array}{r r r r r l l l}
--       ( & [IUnwind], &  addr : addrs, &  values, & (codes', addrs', values') : dump, & heap[addr: NStructure\ t\ fAddrs], & global & )\\
--       \hline
--       ( &    codes', & addr : addrs', & values', &                             dump, & heap, &                              global & )
--     \end{array}
--     \]
--
--     \[
--     \begin{array}{r r r r r l l l}
--       ( & [IUnwind], &          addr : addrs, & values, & dump, & heap[addr: NApplication\ addr_1\ addr_0], & global & )\\
--       \hline
--       ( & [IUnwind], & addr_1 : addr : addrs, & values, & dump, & heap, &                                     global & )
--     \end{array}
--     \]
--
--     \[
--     \begin{array}{r r r r r l l l}
--       ( & [IUnwind], &  addr : addrs, & values, & dump, & heap[addr: NIndirect\ addr'], & global & )\\
--       \hline
--       ( & [IUnwind], & addr' : addrs, & values, & dump, & heap, &                         global & )
--     \end{array}
--     \]
--
--     \[
--     \begin{array}{r r r r r l l l}
--       ( &                                [IUnwind], &    addr_0 : addr_1 : ... : addr_n : addrs, & values, & dump, & heap[addr_0: NConstructor\ t\ n], & global & )\\
--       \hline
--       ( & IRearrange\ (n - 1) : constructorCode\ t, &             addr_1 : ... : addr_n : addrs, & values, & dump, & heap, &                             global & )
--     \end{array}
--     \]
--
--     \[
--     \begin{align}
--     & \begin{array}{r r r r r l l l}
--       ( & [IUnwind], & [addr_0, addr_1, ..., addr_m], & values, & dump, & heap[addr_0: NConstructor\ t\ n], & global & )\\
--       \hline
--       ( &  [Return], & [addr_0, addr_1, ..., addr_m], & values, & dump, & heap, &                             global & )
--     \end{array}\\
--     & \text{(when $m < n$)}
--     \end{align}
--     \]
--
--     \[
--     \begin{array}{r r r r r l l l}
--       ( &                    [IUnwind], &    addr_0 : addr_1 : ... : addr_n : addrs, & values, & dump, & heap[addr_0: NGlobal\ n\ codes'], & global & )\\
--       \hline
--       ( & IRearrange\ (n - 1) : codes', &             addr_1 : ... : addr_n : addrs, & values, & dump, & heap, &                             global & )
--     \end{array}
--     \]
--
--     \[
--     \begin{align}
--     & \begin{array}{r r r r r l l l}
--       ( & [IUnwind], & [addr_0, addr_1, ..., addr_m], & values, & dump, & heap[addr_0: NGlobal\ t\ codes'], & global & )\\
--       \hline
--       ( &  [Return], & [addr_0, addr_1, ..., addr_m], & values, & dump, & heap, &                             global & )
--     \end{array}\\
--     & \text{(when $m < n$)}
--     \end{align}
--     \]
--
-- - __IDestruct__
--
--     \[
--     \begin{array}{r r r r r l l l}
--       ( & IDestruct\ n : codes, &                                 addr : addrs, & values, & dump, & heap[addr: NStructure\ t\ [addr_{n - 1}, ..., addr_1, addr_0]], & global & )\\
--       \hline
--       ( &                codes, & addr_0 : addr_1 : ... : addr_{n - 1} : addrs, & values, & dump, & heap, &                                                           global & )
--     \end{array}
--     \]
--
-- === Dump Related Operations
--
-- - __IEval__
--
--     \[
--     \begin{array}{r r r r r l l l}
--       ( & IEval : codes, & addr : addrs, & values, &                          dump, & heap, & global & )\\
--       \hline
--       ( &     [IUnwind], &       [addr], &     [], & (codes, addrs, values) : dump, & heap, & global & )
--     \end{array}
--     \]
--
-- - __IReturn__
--
--     \[
--     \begin{array}{r r r r r l l l}
--       ( & [IReturn], & [addr_0, addr_1, ..., addr_n], &  values, & (codes', addrs', values') : dump, & heap, & global & )\\
--       \hline
--       ( &    codes', &               addr_n : addrs', & values', &                             dump, & heap, & global & )
--     \end{array}
--     \]
--
-- === Match Operations
--
-- - __IMatch__
--
--     \[
--     \begin{array}{r r r r r l l l}
--       ( & IMatch\ table[t: caseCode] : codes, & addr : addrs, & values, & dump, & heap[addr: NStructure\ t\ fAddrs], & global & )\\
--       \hline
--       ( &                 caseCode <> codes, & addr : addrs, & values, & dump, & heap, &                              global & )
--     \end{array}
--     \]
--
-- === Virtual Operations
-- Following operations are not real constructors of 'Instruction'.
-- These exist only for semantic purposes.
--
-- - __IRearrange__
--
--     \[
--     \begin{array}{r r r r r l l l}
--       ( & IRearrange\ n : codes, &             addr_0 : addr_1 : ... : addr_n : addrs, & values, & dump, & heap[addr_i: NApplication\ addr_i''\ addr_i'], & global & )\\
--       \hline
--       ( &                 codes, & addr_0' : addr_1' : ... : addr_n' : addr_n : addrs, & values, & dump, & heap, &                                         global & )
--     \end{array}\\
--     \]

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
