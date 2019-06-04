{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}
-- |
-- TODO: remove the following option
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module Minicute.Types.Minicute.Annotated.Program
  ( module Minicute.Types.Minicute.Annotated.Expression
  , module Minicute.Types.Minicute.Program


  , AnnotatedSupercombinator
  , MainAnnotatedSupercombinator
  , pattern AnnotatedSupercombinator

  , AnnotatedSupercombinatorL
  , MainAnnotatedSupercombinatorL
  , pattern AnnotatedSupercombinatorL


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

type AnnotatedSupercombinator ann = Supercombinator_ (AnnotatedExpression ann)
type MainAnnotatedSupercombinator ann = AnnotatedSupercombinator ann Identifier
pattern AnnotatedSupercombinator :: Identifier -> [a] -> AnnotatedExpression ann a -> AnnotatedSupercombinator ann a
pattern AnnotatedSupercombinator scId argBinders expr = Supercombinator_ (scId, argBinders, expr)
{-# COMPLETE AnnotatedSupercombinator #-}

type AnnotatedSupercombinatorL ann = Supercombinator_ (AnnotatedExpressionL ann)
type MainAnnotatedSupercombinatorL ann = AnnotatedSupercombinatorL ann Identifier
pattern AnnotatedSupercombinatorL :: Identifier -> [a] -> AnnotatedExpressionL ann a -> AnnotatedSupercombinatorL ann a
pattern AnnotatedSupercombinatorL scId argBinders expr = Supercombinator_ (scId, argBinders, expr)
{-# COMPLETE AnnotatedSupercombinatorL #-}


type AnnotatedProgram ann = Program_ (AnnotatedExpression ann)
type MainAnnotatedProgram ann = AnnotatedProgram ann Identifier
pattern AnnotatedProgram :: [AnnotatedSupercombinator ann a] -> AnnotatedProgram ann a
pattern AnnotatedProgram sc = Program_ sc
{-# COMPLETE AnnotatedProgram #-}

instance {-# OVERLAPS #-} (Show ann, Show a) => Show (AnnotatedProgram ann a) where
  showsPrec p (Program_ scs)
    = showParen (p > appPrec) $ showString "AnnotatedProgram " . showsPrec appPrec1 scs


type AnnotatedProgramL ann = Program_ (AnnotatedExpressionL ann)
type MainAnnotatedProgramL ann = AnnotatedProgramL ann Identifier
pattern AnnotatedProgramL :: [AnnotatedSupercombinatorL ann a] -> AnnotatedProgramL ann a
pattern AnnotatedProgramL sc = Program_ sc
{-# COMPLETE AnnotatedProgramL #-}

instance {-# OVERLAPS #-} (Show ann, Show a) => Show (AnnotatedProgramL ann a) where
  showsPrec p (Program_ scs)
    = showParen (p > appPrec) $ showString "AnnotatedProgramL " . showsPrec appPrec1 scs
