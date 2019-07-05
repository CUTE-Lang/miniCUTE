{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Types for miniCUTE programs
module Minicute.Data.Minicute.Program
  ( module Minicute.Data.Minicute.Expression

  , Supercombinator_( .. )

  , Supercombinator
  , MainSupercombinator
  , pattern Supercombinator

  , SupercombinatorL
  , MainSupercombinatorL
  , pattern SupercombinatorL

  , _supercombinatorBinder
  , _supercombinatorArguments
  , _supercombinatorBody


  , Program_( .. )

  , Program
  , MainProgram
  , pattern Program


  , ProgramL
  , MainProgramL
  , pattern ProgramL
  ) where

import Control.Lens.TH
import Control.Lens.Tuple
import Control.Lens.Type
import Control.Lens.Wrapped
import Data.Data
import GHC.Generics
import GHC.Show ( appPrec, appPrec1 )
import Language.Haskell.TH.Syntax
import Minicute.Data.Minicute.Expression
import Data.Text.Prettyprint.Doc ( Pretty( .. ) )

import qualified Data.Text.Prettyprint.Doc as PP

-- |
-- An internal type for a supercombinator (top-level function definition)
--
-- [@Identifier@] the top-level identifier of the definition.
--
-- [@[a\]@] the arguments of the definition.
--
-- [@expr a@] the body expression of the definition.
newtype Supercombinator_ expr a
  = Supercombinator_ (Identifier, [a], expr a)
  deriving ( Generic
           , Typeable
           , Data
           , Lift
           , Eq
           , Ord
           , Show
           )

-- |
-- 'Supercombinator_' of 'Expression'
type Supercombinator = Supercombinator_ Expression
-- |
-- 'Supercombinator_' of 'MainExpression'
type MainSupercombinator = Supercombinator Identifier
-- |
-- Utility pattern for 'Supercombinator'
pattern Supercombinator :: Identifier -> [a] -> Expression a -> Supercombinator a
pattern Supercombinator scId argBinders expr = Supercombinator_ (scId, argBinders, expr)
{-# COMPLETE Supercombinator #-}

-- |
-- 'Supercombinator_' of 'ExpressionL'
type SupercombinatorL = Supercombinator_ ExpressionL
-- |
-- 'Supercombinator_' of 'MainExpressionL'
type MainSupercombinatorL = SupercombinatorL Identifier
-- |
-- Utility pattern for 'SupercombinatorL'
pattern SupercombinatorL :: Identifier -> [a] -> ExpressionL a -> SupercombinatorL a
pattern SupercombinatorL scId argBinders expr = Supercombinator_ (scId, argBinders, expr)
{-# COMPLETE SupercombinatorL #-}

instance (Pretty a, Pretty (expr a)) => Pretty (Supercombinator_ expr a) where
  pretty (Supercombinator_ (scId, argBinders, expr))
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
-- An internal type for a miniCUTE program
newtype Program_ expr a
  = Program_ [Supercombinator_ expr a]
  deriving ( Generic
           , Typeable
           , Data
           , Lift
           , Eq
           , Ord
           , Show
           )

-- |
-- 'Program_' of 'Expression'
type Program = Program_ Expression
-- |
-- 'Program_' of 'MainExpression'
type MainProgram = Program Identifier
-- |
-- Utility pattern for 'Program'
pattern Program :: [Supercombinator a] -> Program a
pattern Program sc = Program_ sc
{-# COMPLETE Program #-}

instance {-# OVERLAPS #-} (Show a) => Show (Program a) where
  showsPrec p (Program_ scs)
    = showParen (p > appPrec) $ showString "Program " . showsPrec appPrec1 scs

instance (Pretty a, Pretty (expr a)) => Pretty (Program_ expr a) where
  pretty (Program_ scs) = PP.vcat . PP.punctuate PP.semi . fmap pretty $ scs


-- |
-- 'Program_' of 'ExpressionL'
type ProgramL = Program_ ExpressionL
-- |
-- 'Program_' of 'MainExpressionL'
type MainProgramL = ProgramL Identifier
-- |
-- Utility pattern for 'ProgramL'
pattern ProgramL :: [SupercombinatorL a] -> ProgramL a
pattern ProgramL sc = Program_ sc
{-# COMPLETE ProgramL #-}

instance {-# OVERLAPS #-} (Show a) => Show (ProgramL a) where
  showsPrec p (Program_ scs)
    = showParen (p > appPrec) $ showString "ProgramL " . showsPrec appPrec1 scs


makeWrapped ''Supercombinator_

-- |
-- 'Lens' to extract the binder of 'Supercombinator_'
_supercombinatorBinder :: Lens' (Supercombinator_ expr a) Identifier
_supercombinatorBinder = _Wrapped . _1
{-# INLINEABLE _supercombinatorBinder #-}

-- |
-- 'Lens' to extract the list of arguments of 'Supercombinator_'
_supercombinatorArguments :: Lens' (Supercombinator_ expr a) [a]
_supercombinatorArguments = _Wrapped . _2
{-# INLINEABLE _supercombinatorArguments #-}

-- |
-- 'Lens' to extract the body expression of 'Supercombinator_'
_supercombinatorBody :: Lens (Supercombinator_ expr1 a) (Supercombinator_ expr2 a) (expr1 a) (expr2 a)
_supercombinatorBody = _Wrapped . _3
{-# INLINEABLE _supercombinatorBody #-}


makeWrapped ''Program_
