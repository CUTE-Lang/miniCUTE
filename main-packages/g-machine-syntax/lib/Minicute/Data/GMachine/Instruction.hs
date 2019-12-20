{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
-- |
-- Copyright: (c) 2018-present Junyoung Clare Jang
-- License: BSD 3-Clause
--
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
  , MatchTable( .. )
  , MatchEntry( .. )
  ) where

import Data.Data ( Data, Typeable )
import GHC.Generics ( Generic )
import Language.Haskell.TH.Syntax
import Minicute.Data.Common

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
-- The following tuple describes the initial state for the machine.
--
--     \[
--     \begin{align}
--     & \begin{array}{r r r r r l l l}
--       ( & [\text{IMakeGlobal}\ \unicode{x201C}main\unicode{x201D}, \text{IEval}], & [], & [], & [], & initialHeap, & initialGlobal & )
--     \end{array}\\
--     & \text{(when $initialHeap$ is a heap including all NGlobals for the supercombinators in a program)}\\
--     & \text{(when $initialGlobal$ is a global including all supercombinators in a program)}
--     \end{align}
--     \]
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
--       ( & \text{IMakeInteger}\ n : codes, &        addrs, & values, & dump, & heap, &                           global & )\\
--       \hline
--       ( &                          codes, & addr : addrs, & values, & dump, & heap[addr: \text{NInteger}\ n], & global & )
--     \end{array}
--     \]
--
-- - __IMakeConstructor__
--
--
--     \[
--     \begin{array}{r r r r r l l l}
--       ( & \text{IMakeConstructor}\ t\ n : codes, &        addrs, & values, & dump, & heap, &                                  global & )\\
--       \hline
--       ( &                                 codes, & addr : addrs, & values, & dump, & heap[addr: \text{NConstructor}\ t\ n], & global & )
--     \end{array}
--     \]
--
-- - __IMakeStructure__
--
--     \[
--     \begin{array}{r r r r r l l l}
--       ( & \text{IMakeStructure}\ t\ n : codes, & addr_0 : addr_1 : ... : addr_{n - 1} : addrs, & values, & dump, & heap, &                                                                  global & )\\
--       \hline
--       ( &                               codes, &                                 addr : addrs, & values, & dump, & heap[addr: \text{NStructure}\ t\ [addr_{n - 1}, ..., addr_1, addr_0]], & global & )
--     \end{array}
--     \]
--
-- - __IMakeApplication__
--
--     \[
--     \begin{array}{r r r r r l l l}
--       ( & \text{IMakeApplication} : codes, & addr_0 : addr_1 : addrs, & values, & dump, & heap, &                                            global & )\\
--       \hline
--       ( &                           codes, &            addr : addrs, & values, & dump, & heap[addr: \text{NApplication}\ addr_1\ addr_0], & global & )
--     \end{array}
--     \]
--
-- - __IMakeGlobal__
--
--     \[
--     \begin{array}{r r r r r l l l}
--       ( & \text{IMakeGlobal}\ id : codes, &        addrs, & values, & dump, & heap, & global[id: addr] & )\\
--       \hline
--       ( &                          codes, & addr : addrs, & values, & dump, & heap, & global[id: addr] & )
--     \end{array}
--     \]
--
-- - __IMakePlaceholders__
--
--     \[
--     \begin{align}
--     & \begin{array}{r r r r r l l l}
--       ( & \text{IMakePlaceholders}\ n : codes, &                                  addrs, & values, & dump, & heap, &  global & )\\
--       \hline
--       ( &                               codes, & addr_0 : addr_1 : ... : addr_n : addrs, & values, & dump, & heap', & global & )
--     \end{array}\\
--     & heap' = heap[addr_0: \text{NEmpty}, addr_1: \text{NEmpty}, ..., addr_n: \text{NEmpty}]
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
--       ( & \text{IPop}\ n : codes, & addr_0 : addr_1 : ... : addr_{n - 1} : addrs, & values, & dump, & heap, & global & )\\
--       \hline
--       ( &                  codes, &                                        addrs, & values, & dump, & heap, & global & )
--     \end{array}
--     \]
--
-- - __IDig__
--
--     \[
--     \begin{array}{r r r r r l l l}
--       ( & \text{IDig}\ n : codes, & addr_0 : addr_1 : ... : addr_n : addrs, & values, & dump, & heap, & global & )\\
--       \hline
--       ( &                  codes, &                         addr_0 : addrs, & values, & dump, & heap, & global & )
--     \end{array}
--     \]
--
-- - __IUpdate__
--
--     \[
--     \begin{array}{r r r r r l l l}
--       ( & \text{IUpdate}\ n : codes, & addr_0 : addr_1 : ... : addr_n : addrs, & values, & dump, & heap, &                                   global & )\\
--       \hline
--       ( &                     codes, & addr_0 : addr_1 : ... : addr_n : addrs, & values, & dump, & heap[addr_n: \text{NIndirect}\ addr_0], & global & )
--     \end{array}
--     \]
--
-- - __ICopy__
--
--     \[
--     \begin{array}{r r r r r l l l}
--       ( & \text{ICopy}\ n : codes, &          addr_0 : addr_1 : ... : addr_n : addrs, & values, & dump, & heap, & global & )\\
--       \hline
--       ( &                   codes, & addr_n : addr_0 : addr_1 : ... : addr_n : addrs, & values, & dump, & heap, & global & )
--     \end{array}
--     \]
--
-- === Value Stack Based Operations
-- - __IPushBasicValue__
--
--     \[
--     \begin{array}{r r r r r l l l}
--       ( & \text{IPushBasicValue}\ v : codes, & addrs, &     values, & dump, & heap, & global & )\\
--       \hline
--       ( &                             codes, & addrs, & v : values, & dump, & heap, & global & )
--     \end{array}
--     \]
--
-- - __IPushExtractedValue__
--
--     \[
--     \begin{array}{r r r r r l l l}
--       ( & \text{IPushExtractedValue} : codes, & addr : addrs, &     values, & dump, & heap[addr: \text{NInteger}\ v], & global & )\\
--       \hline
--       ( &                              codes, &        addrs, & v : values, & dump, & heap, &                           global & )
--     \end{array}
--     \]
--
--     \[
--     \begin{array}{r r r r r l l l}
--       ( & \text{IPushExtractedValue} : codes, & addr : addrs, &     values, & dump, & heap[addr: \text{NStructure}\ v\ []], & global & )\\
--       \hline
--       ( &                              codes, &        addrs, & v : values, & dump, & heap, &                                 global & )
--     \end{array}
--     \]
--
-- - __IWrapAsInteger__
--
--     \[
--     \begin{array}{r r r r r l l l}
--       ( & \text{IWrapAsInteger} : codes, &        addrs, & v : values, & dump, & heap, &                           global & )\\
--       \hline
--       ( &                         codes, & addr : addrs, &     values, & dump, & heap[addr: \text{NInteger}\ v], & global & )
--     \end{array}
--     \]
--
-- - __IWrapAsStructure__
--
--     \[
--     \begin{array}{r r r r r l l l}
--       ( & \text{IWrapAsStructure} : codes, &        addrs, & v : values, & dump, & heap, &                                 global & )\\
--       \hline
--       ( &                           codes, & addr : addrs, &     values, & dump, & heap[addr: \text{NStructure}\ v\ []], & global & )
--     \end{array}
--     \]
--
-- - __IUpdateAsInteger__
--
--     \[
--     \begin{array}{r r r r r l l l}
--       ( & \text{IUpdateAsInteger}\ n : codes, & addr_0 : addr_1 : ... : addr_n : addrs, & v : values, & dump, & heap, &                             global & )\\
--       \hline
--       ( &                              codes, & addr_0 : addr_1 : ... : addr_n : addrs, &     values, & dump, & heap[addr_n: \text{NInteger}\ v], & global & )
--     \end{array}
--     \]
--
-- - __IUpdateAsStructure__
--
--     \[
--     \begin{array}{r r r r r l l l}
--       ( & \text{IUpdateAsStructure}\ n : codes, & addr_0 : addr_1 : ... : addr_n : addrs, & v : values, & dump, & heap, &                                   global & )\\
--       \hline
--       ( &                                codes, & addr_0 : addr_1 : ... : addr_n : addrs, &     values, & dump, & heap[addr_n: \text{NStructure}\ v\ []], & global & )
--     \end{array}
--     \]
--
-- === Primitive Operations
-- - __IPrimitive__
--
--     \[
--     \begin{align}
--     & \begin{array}{r r r r r l l l}
--       ( & \text{IPrimitive}\ op : codes, & addrs, & v_0 : v_1 : values, & dump, & heap, & global & )\\
--       \hline
--       ( &                         codes, & addrs, &        v' : values, & dump, & heap, & global & )
--     \end{array}\\
--     & v' = v_0\ R\ v1\\
--     & \text{(when $op$ represents a binary operation $R$)}
--     \end{align}
--     \]
--
--     \[
--     \begin{align}
--     & \begin{array}{r r r r r l l l}
--       ( & \text{IPrimitive}\ op : codes, & addrs, &  v : values, & dump, & heap, & global & )\\
--       \hline
--       ( &                         codes, & addrs, & v' : values, & dump, & heap, & global & )
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
--       ( & [\text{IUnwind}], &  addr : addrs, &  values, & (codes', addrs', values') : dump, & heap[addr: \text{NInteger}\ n], & global & )\\
--       \hline
--       ( &           codes', & addr : addrs', & values', &                             dump, & heap, &                           global & )
--     \end{array}
--     \]
--
--     \[
--     \begin{array}{r r r r r l l l}
--       ( & [\text{IUnwind}], &  addr : addrs, &  values, & (codes', addrs', values') : dump, & heap[addr: \text{NStructure}\ t\ fAddrs], & global & )\\
--       \hline
--       ( &           codes', & addr : addrs', & values', &                             dump, & heap, &                                     global & )
--     \end{array}
--     \]
--
--     \[
--     \begin{array}{r r r r r l l l}
--       ( & [\text{IUnwind}], &          addr : addrs, & values, & dump, & heap[addr: \text{NApplication}\ addr_1\ addr_0], & global & )\\
--       \hline
--       ( & [\text{IUnwind}], & addr_1 : addr : addrs, & values, & dump, & heap, &                                            global & )
--     \end{array}
--     \]
--
--     \[
--     \begin{array}{r r r r r l l l}
--       ( & [\text{IUnwind}], &  addr : addrs, & values, & dump, & heap[addr: \text{NIndirect}\ addr'], & global & )\\
--       \hline
--       ( & [\text{IUnwind}], & addr' : addrs, & values, & dump, & heap, &                                global & )
--     \end{array}
--     \]
--
--     \[
--     \begin{array}{r r r r r l l l}
--       ( &          [\text{IUnwind}], & addr : addrs, & values, & dump, & heap[addr: \text{NConstructor}\ t\ 0], & global & )\\
--       \hline
--       ( & \text{constructorCode}\ t, & addr : addrs, & values, & dump, & heap, &                                  global & )
--     \end{array}
--     \]
--
--     \[
--     \begin{align}
--     & \begin{array}{r r r r r l l l}
--       ( &                                 [\text{IUnwind}], & addr_0 : addr_1 : ... : addr_n : addrs, & values, & dump, & heap[addr_0: \text{NConstructor}\ t\ n], & global & )\\
--       \hline
--       ( & \text{IRearrange}\ n : \text{constructorCode}\ t, &          addr_1 : ... : addr_n : addrs, & values, & dump, & heap, &                                    global & )
--     \end{array}\\
--     & \text{(when $n > 0$)}
--     \end{align}
--     \]
--
--     \[
--     \begin{align}
--     & \begin{array}{r r r r r l l l}
--       ( & [\text{IUnwind}], & [addr_0, addr_1, ..., addr_m], & values, & dump, & heap[addr_0: \text{NConstructor}\ t\ n], & global & )\\
--       \hline
--       ( &  [\text{Return}], & [addr_0, addr_1, ..., addr_m], & values, & dump, & heap, &                                    global & )
--     \end{array}\\
--     & \text{(when $m < n$)}
--     \end{align}
--     \]
--
--     \[
--     \begin{array}{r r r r r l l l}
--       ( & [\text{IUnwind}], & addr : addrs, & values, & dump, & heap[addr: \text{NGlobal}\ 0\ codes'], & global & )\\
--       \hline
--       ( &           codes', & addr : addrs, & values, & dump, & heap, &                                  global & )
--     \end{array}
--     \]
--
--     \[
--     \begin{align}
--     & \begin{array}{r r r r r l l l}
--       ( &              [\text{IUnwind}], & addr_0 : addr_1 : ... : addr_n : addrs, & values, & dump, & heap[addr_0: \text{NGlobal}\ n\ codes'], & global & )\\
--       \hline
--       ( & \text{IRearrange}\ n : codes', &          addr_1 : ... : addr_n : addrs, & values, & dump, & heap, &                                    global & )
--     \end{array}\\
--     & \text{(when $n > 0$)}
--     \end{align}
--     \]
--
--     \[
--     \begin{align}
--     & \begin{array}{r r r r r l l l}
--       ( & [\text{IUnwind}], & [addr_0, addr_1, ..., addr_m], & values, & dump, & heap[addr_0: \text{NGlobal}\ t\ codes'], & global & )\\
--       \hline
--       ( &  [\text{Return}], & [addr_0, addr_1, ..., addr_m], & values, & dump, & heap, &                                    global & )
--     \end{array}\\
--     & \text{(when $m < n$)}
--     \end{align}
--     \]
--
-- - __IDestruct__
--
--     \[
--     \begin{array}{r r r r r l l l}
--       ( & \text{IDestruct}\ n : codes, &                                 addr : addrs, & values, & dump, & heap[addr: \text{NStructure}\ t\ [addr_{n - 1}, ..., addr_1, addr_0]], & global & )\\
--       \hline
--       ( &                       codes, & addr_0 : addr_1 : ... : addr_{n - 1} : addrs, & values, & dump, & heap, &                                                                  global & )
--     \end{array}
--     \]
--
-- === Dump Related Operations
--
-- - __IEval__
--
--     \[
--     \begin{array}{r r r r r l l l}
--       ( & \text{IEval} : codes, & addr : addrs, & values, &                          dump, & heap, & global & )\\
--       \hline
--       ( &     [\text{IUnwind}], &       [addr], &     [], & (codes, addrs, values) : dump, & heap, & global & )
--     \end{array}
--     \]
--
-- - __IReturn__
--
--     \[
--     \begin{array}{r r r r r l l l}
--       ( & [\text{IReturn}], & [addr_0, addr_1, ..., addr_n], &  values, & (codes', addrs', values') : dump, & heap, & global & )\\
--       \hline
--       ( &           codes', &               addr_n : addrs', & values', &                             dump, & heap, & global & )
--     \end{array}
--     \]
--
-- === Match Operations
--
-- - __IMatch__
--
--     \[
--     \begin{array}{r r r r r l l l}
--       ( & \text{IMatch}\ table[t: casecode_t] : codes, & addr : addrs, & values, & dump, & heap[addr: \text{NStructure}\ t\ fAddrs], & global & )\\
--       \hline
--       ( &                   casecode_t \diamond codes, & addr : addrs, & values, & dump, & heap, &                                     global & )
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
--       ( & \text{IRearrange}\ n : codes, &                    addr_0 : addr_1 : ... : addr_{n - 1} : addrs, & values, & dump, & heap[addr_i: \text{NApplication}\ addr'_i\ addr''_i], & global & )\\
--       \hline
--       ( &                        codes, & addr''_0 : addr''_1 : ... : addr''_{n - 1} : addr_{n - 1} : addrs, & values, & dump, & heap, &                                               global & )
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
  | IPrimitive Primitive

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
