{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}
-- |
-- TODO: remove the following option
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
module Minicute.Types.Minicute.Annotated.Program
  ( module Minicute.Types.Minicute.Annotated.Expression
  , module Minicute.Types.Minicute.Program


  , AnnotatedSupercombinator
  , MainAnnotatedSupercombinator

  , AnnotatedSupercombinatorL
  , MainAnnotatedSupercombinatorL


  , AnnotatedProgram
  , MainAnnotatedProgram
  , pattern AnnotatedProgram


  , AnnotatedProgramL
  , MainAnnotatedProgramL
  , pattern AnnotatedProgramL
  ) where

import GHC.Show ( appPrec, appPrec1 )
import Minicute.Types.Minicute.Annotated.Expression
import Minicute.Types.Minicute.Program

type AnnotatedSupercombinator ann a = Supercombinator_ (AnnotatedExpression ann) a
type MainAnnotatedSupercombinator ann = AnnotatedSupercombinator ann Identifier

type AnnotatedSupercombinatorL ann a = Supercombinator_ (AnnotatedExpressionL ann) a
type MainAnnotatedSupercombinatorL ann = AnnotatedSupercombinatorL ann Identifier


type AnnotatedProgram ann = Program_ (AnnotatedExpression ann)
type MainAnnotatedProgram ann = AnnotatedProgram ann Identifier
pattern AnnotatedProgram sc = Program_ sc
{-# COMPLETE AnnotatedProgram #-}

instance {-# OVERLAPS #-} (Show ann, Show a) => Show (AnnotatedProgram ann a) where
  showsPrec p (Program_ scs)
    = showParen (p > appPrec) $ showString "AnnotatedProgram " . showsPrec appPrec1 scs


type AnnotatedProgramL ann = Program_ (AnnotatedExpressionL ann)
type MainAnnotatedProgramL ann = AnnotatedProgramL ann Identifier
pattern AnnotatedProgramL sc = Program_ sc
{-# COMPLETE AnnotatedProgramL #-}

instance {-# OVERLAPS #-} (Show ann, Show a) => Show (AnnotatedProgramL ann a) where
  showsPrec p (Program_ scs)
    = showParen (p > appPrec) $ showString "AnnotatedProgramL " . showsPrec appPrec1 scs
