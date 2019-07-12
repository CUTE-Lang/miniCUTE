{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Types for miniCUTE programs
module Minicute.Data.Minicute.Program
  ( module Minicute.Data.Minicute.Expression

  , Supercombinator( .. )
  , MainSupercombinator
  , MainSupercombinatorL

  , _supercombinatorBinder
  , _supercombinatorArguments
  , _supercombinatorBody


  , Program( .. )
  , MainProgram
  , MainProgramL
  ) where

import Control.Lens.TH
import Control.Lens.Tuple
import Control.Lens.Type
import Control.Lens.Wrapped
import Data.Data
import GHC.Generics
import Language.Haskell.TH.Syntax
import Minicute.Data.Minicute.Expression
import Data.Text.Prettyprint.Doc ( Pretty( .. ) )

import qualified Data.Text.Prettyprint.Doc as PP

-- |
-- A type for a supercombinator (top-level function definition)
--
-- [@Identifier@] the top-level identifier of the definition.
--
-- [@[a\]@] the arguments of the definition.
--
-- [@expr a@] the body expression of the definition.
newtype Supercombinator expr a
  = Supercombinator (Identifier, [a], expr a)
  deriving ( Generic
           , Typeable
           , Data
           , Lift
           , Eq
           , Ord
           , Show
           )
-- |
-- A supercombinator of 'MainExpression'
type MainSupercombinator = Supercombinator (Expression 'LLMC) Identifier
-- |
-- A supercombinator of 'MainExpressionL'
type MainSupercombinatorL = Supercombinator (Expression 'MC) Identifier

instance (Pretty a, Pretty (expr a)) => Pretty (Supercombinator expr a) where
  pretty (Supercombinator (scId, argBinders, expr))
    = PP.hcat
      [ pretty scId
      , if null argBinders
        then PP.emptyDoc
        else PP.space
      , PP.hsep . fmap pretty $ argBinders
      , PP.space
      , PP.equals
      , PP.space
      , pretty expr
      ]


-- |
-- A type for a miniCUTE program
newtype Program expr a
  = Program [Supercombinator expr a]
  deriving ( Generic
           , Typeable
           , Data
           , Lift
           , Eq
           , Ord
           , Show
           )
-- |
-- A program of 'MainExpression'
type MainProgram = Program (Expression 'LLMC) Identifier
-- |
-- 'Program_' of 'MainExpressionL'
type MainProgramL = Program (Expression 'MC) Identifier

instance (Pretty a, Pretty (expr a)) => Pretty (Program expr a) where
  pretty (Program scs) = PP.vcat . PP.punctuate PP.semi . fmap pretty $ scs


makeWrapped ''Supercombinator

-- |
-- 'Lens' to extract the binder of 'Supercombinator'
_supercombinatorBinder :: Lens' (Supercombinator expr a) Identifier
_supercombinatorBinder = _Wrapped . _1
{-# INLINEABLE _supercombinatorBinder #-}

-- |
-- 'Lens' to extract the list of arguments of 'Supercombinator'
_supercombinatorArguments :: Lens' (Supercombinator expr a) [a]
_supercombinatorArguments = _Wrapped . _2
{-# INLINEABLE _supercombinatorArguments #-}

-- |
-- 'Lens' to extract the body expression of 'Supercombinator'
_supercombinatorBody :: Lens (Supercombinator expr1 a) (Supercombinator expr2 a) (expr1 a) (expr2 a)
_supercombinatorBody = _Wrapped . _3
{-# INLINEABLE _supercombinatorBody #-}


makeWrapped ''Program
