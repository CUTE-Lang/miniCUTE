{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module Minicute.Types.Minicute.Program
  ( module Minicute.Types.Minicute.Expression

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


  , _supercombinators
  ) where

import Control.Lens.Lens ( lens )
import Control.Lens.Tuple
import Control.Lens.Type
import Data.Data
import GHC.Generics
import GHC.Show ( appPrec, appPrec1 )
import Language.Haskell.TH.Syntax
import Minicute.Types.Minicute.Expression
import Data.Text.Prettyprint.Doc ( Pretty( .. ) )

import qualified Data.Text.Prettyprint.Doc as PP

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

type Supercombinator a = Supercombinator_ Expression a
type MainSupercombinator = Supercombinator Identifier
pattern Supercombinator :: Identifier -> [a] -> Expression a -> Supercombinator a
pattern Supercombinator scId argBinders expr = Supercombinator_ (scId, argBinders, expr)
{-# COMPLETE Supercombinator #-}

type SupercombinatorL a = Supercombinator_ ExpressionL a
type MainSupercombinatorL = SupercombinatorL Identifier
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

_supercombinator :: Lens (Supercombinator_ expr1 a) (Supercombinator_ expr2 a) (Identifier, [a], expr1 a) (Identifier, [a], expr2 a)
_supercombinator = lens getter setter
  where
    getter (Supercombinator_ sc) = sc
    setter _ = Supercombinator_

_supercombinatorBinder :: Lens' (Supercombinator_ expr a) Identifier
_supercombinatorBinder = _supercombinator . _1
{-# INLINEABLE _supercombinatorBinder #-}

_supercombinatorArguments :: Lens' (Supercombinator_ expr a) [a]
_supercombinatorArguments = _supercombinator . _2
{-# INLINEABLE _supercombinatorArguments #-}

_supercombinatorBody :: Lens (Supercombinator_ expr1 a) (Supercombinator_ expr2 a) (expr1 a) (expr2 a)
_supercombinatorBody = _supercombinator . _3
{-# INLINEABLE _supercombinatorBody #-}


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

type Program = Program_ Expression
type MainProgram = Program Identifier
pattern Program sc = Program_ sc
{-# COMPLETE Program #-}

instance {-# OVERLAPS #-} (Show a) => Show (Program a) where
  showsPrec p (Program_ scs)
    = showParen (p > appPrec) $ showString "Program " . showsPrec appPrec1 scs

instance (Pretty a, Pretty (expr a)) => Pretty (Program_ expr a) where
  pretty (Program_ scs) = PP.vcat . PP.punctuate PP.semi . fmap pretty $ scs


type ProgramL = Program_ ExpressionL
type MainProgramL = ProgramL Identifier
pattern ProgramL sc = Program_ sc
{-# COMPLETE ProgramL #-}

instance {-# OVERLAPS #-} (Show a) => Show (ProgramL a) where
  showsPrec p (Program_ scs)
    = showParen (p > appPrec) $ showString "ProgramL " . showsPrec appPrec1 scs

_supercombinators :: Lens (Program_ expr a) (Program_ expr' a') [Supercombinator_ expr a] [Supercombinator_ expr' a']
_supercombinators = lens getter setter
  where
    getter (Program_ scs) = scs
    setter _ = Program_
{-# INLINEABLE _supercombinators #-}
