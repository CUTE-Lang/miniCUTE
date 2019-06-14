{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
-- |
-- Types for annotated programs
--
-- __TODO: remove the /-fno-warn-orphans/ option__
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

-- |
-- @AnnotatedSupercombinator ann a@ is a @Supercombinator@ annotated by @ann@.
type AnnotatedSupercombinator ann = Supercombinator_ (AnnotatedExpression ann)
-- |
-- @MainAnnotatedSupercombinator ann@ is a @MainSupercombinator@ annotated by @ann@.
type MainAnnotatedSupercombinator ann = AnnotatedSupercombinator ann Identifier
-- |
-- Utility pattern for 'AnnotatedSupercombinator'
pattern AnnotatedSupercombinator :: Identifier -> [a] -> AnnotatedExpression ann a -> AnnotatedSupercombinator ann a
pattern AnnotatedSupercombinator scId argBinders expr = Supercombinator_ (scId, argBinders, expr)
{-# COMPLETE AnnotatedSupercombinator #-}

-- |
-- @AnnotatedSupercombinatorL ann a@ is a @SupercombinatorL@ annotated by @ann@.
type AnnotatedSupercombinatorL ann = Supercombinator_ (AnnotatedExpressionL ann)
-- |
-- @MainAnnotatedSupercombinatorL ann@ is a @MainSupercombinatorL@ annotated by @ann@.
type MainAnnotatedSupercombinatorL ann = AnnotatedSupercombinatorL ann Identifier
-- |
-- Utility pattern for 'AnnotatedSupercombinatorL'
pattern AnnotatedSupercombinatorL :: Identifier -> [a] -> AnnotatedExpressionL ann a -> AnnotatedSupercombinatorL ann a
pattern AnnotatedSupercombinatorL scId argBinders expr = Supercombinator_ (scId, argBinders, expr)
{-# COMPLETE AnnotatedSupercombinatorL #-}


-- |
-- @AnnotatedProgram ann a@ is a @Program@ annotated by @ann@.
type AnnotatedProgram ann = Program_ (AnnotatedExpression ann)
-- |
-- @MainAnnotatedProgram ann a@ is a @MainProgram@ annotated by @ann@.
type MainAnnotatedProgram ann = AnnotatedProgram ann Identifier
-- |
-- Utility pattern for 'AnnotatedProgram'
pattern AnnotatedProgram :: [AnnotatedSupercombinator ann a] -> AnnotatedProgram ann a
pattern AnnotatedProgram sc = Program_ sc
{-# COMPLETE AnnotatedProgram #-}

instance {-# OVERLAPS #-} (Show ann, Show a) => Show (AnnotatedProgram ann a) where
  showsPrec p (Program_ scs)
    = showParen (p > appPrec) $ showString "AnnotatedProgram " . showsPrec appPrec1 scs


-- |
-- @AnnotatedProgramL ann a@ is a @ProgramL@ annotated by @ann@.
type AnnotatedProgramL ann = Program_ (AnnotatedExpressionL ann)
-- |
-- @MainAnnotatedProgramL ann a@ is a @MainProgramL@ annotated by @ann@.
type MainAnnotatedProgramL ann = AnnotatedProgramL ann Identifier
-- |
-- Utility pattern for 'AnnotatedProgramL'
pattern AnnotatedProgramL :: [AnnotatedSupercombinatorL ann a] -> AnnotatedProgramL ann a
pattern AnnotatedProgramL sc = Program_ sc
{-# COMPLETE AnnotatedProgramL #-}

instance {-# OVERLAPS #-} (Show ann, Show a) => Show (AnnotatedProgramL ann a) where
  showsPrec p (Program_ scs)
    = showParen (p > appPrec) $ showString "AnnotatedProgramL " . showsPrec appPrec1 scs
