{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}
-- |
-- TODO: remove the following option
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
module Minicute.Types.Minicute.Program
  ( module Minicute.Types.Minicute.Expression

  , Supercombinator_

  , Supercombinator
  , MainSupercombinator

  , SupercombinatorL
  , MainSupercombinatorL

  , AnnotatedSupercombinator
  , MainAnnotatedSupercombinator

  , AnnotatedSupercombinatorL
  , MainAnnotatedSupercombinatorL

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


  , AnnotatedProgram
  , MainAnnotatedProgram
  , pattern AnnotatedProgram


  , AnnotatedProgramL
  , MainAnnotatedProgramL
  , pattern AnnotatedProgramL


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
import Data.Text.Prettyprint.Doc ( Pretty(..) )

import qualified Data.Text.Prettyprint.Doc as PP

type Supercombinator_ expr a = (Identifier, [a], expr a)

type Supercombinator a = Supercombinator_ Expression a
type MainSupercombinator = Supercombinator Identifier

type SupercombinatorL a = Supercombinator_ ExpressionL a
type MainSupercombinatorL = SupercombinatorL Identifier

type AnnotatedSupercombinator ann a = Supercombinator_ (AnnotatedExpression ann) a
type MainAnnotatedSupercombinator ann = AnnotatedSupercombinator ann Identifier

type AnnotatedSupercombinatorL ann a = Supercombinator_ (AnnotatedExpressionL ann) a
type MainAnnotatedSupercombinatorL ann = AnnotatedSupercombinatorL ann Identifier

instance {-# OVERLAPS #-} (Pretty a, Pretty (expr a)) => Pretty (Supercombinator_ expr a) where
  pretty (scId, argBinders, expr)
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

_supercombinatorBinder :: Lens' (Supercombinator_ expr a) Identifier
_supercombinatorBinder = _1
{-# INLINEABLE _supercombinatorBinder #-}

_supercombinatorArguments :: Lens' (Supercombinator_ expr a) [a]
_supercombinatorArguments = _2
{-# INLINEABLE _supercombinatorArguments #-}

_supercombinatorBody :: Lens (Supercombinator_ expr1 a) (Supercombinator_ expr2 a) (expr1 a) (expr2 a)
_supercombinatorBody = _3
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
  showsPrec = showProgram_ "Program "

instance (Pretty a, Pretty (expr a)) => Pretty (Program_ expr a) where
  pretty (Program_ scs) = PP.vcat . PP.punctuate PP.semi . fmap pretty $ scs


type ProgramL = Program_ ExpressionL
type MainProgramL = ProgramL Identifier
pattern ProgramL sc = Program_ sc
{-# COMPLETE ProgramL #-}

instance {-# OVERLAPS #-} (Show a) => Show (ProgramL a) where
  showsPrec = showProgram_ "ProgramL "


type AnnotatedProgram ann = Program_ (AnnotatedExpression ann)
type MainAnnotatedProgram ann = AnnotatedProgram ann Identifier
pattern AnnotatedProgram sc = Program_ sc
{-# COMPLETE AnnotatedProgram #-}

instance {-# OVERLAPS #-} (Show ann, Show a) => Show (AnnotatedProgram ann a) where
  showsPrec = showProgram_ "AnnotatedProgram "


type AnnotatedProgramL ann = Program_ (AnnotatedExpressionL ann)
type MainAnnotatedProgramL ann = AnnotatedProgramL ann Identifier
pattern AnnotatedProgramL sc = Program_ sc
{-# COMPLETE AnnotatedProgramL #-}

instance {-# OVERLAPS #-} (Show ann, Show a) => Show (AnnotatedProgramL ann a) where
  showsPrec = showProgram_ "AnnotatedProgramL "

showProgram_ :: (Show a, Show (expr a)) => String -> Int -> Program_ expr a -> ShowS
showProgram_ name p (Program_ scs)
  = showParen (p > appPrec)
    $ showString name . showsPrec appPrec1 scs
{-# INLINEABLE showProgram_ #-}

_supercombinators :: Lens (Program_ expr a) (Program_ expr' a') [Supercombinator_ expr a] [Supercombinator_ expr' a']
_supercombinators = lens getter setter
  where
    getter (Program_ scs) = scs
    setter _ = Program_
{-# INLINEABLE _supercombinators #-}
