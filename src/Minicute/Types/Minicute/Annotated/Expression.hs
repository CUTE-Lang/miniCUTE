{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Types for annotated expressions
module Minicute.Types.Minicute.Annotated.Expression
  ( module Minicute.Data.Fix
  , module Minicute.Types.Minicute.Common
  , module Minicute.Types.Minicute.Expression


  , AnnotatedLetDefinition
  , MainAnnotatedLetDefinition
  , pattern AnnotatedLetDefinition

  , AnnotatedLetDefinitionL
  , MainAnnotatedLetDefinitionL
  , pattern AnnotatedLetDefinitionL


  , AnnotatedMatchCase
  , MainAnnotatedMatchCase
  , pattern AnnotatedMatchCase

  , AnnotatedMatchCaseL
  , MainAnnotatedMatchCaseL
  , pattern AnnotatedMatchCaseL


  , AnnotatedExpression_( .. )

  , AnnotatedExpression
  , MainAnnotatedExpression
  , pattern AnnotatedExpression
  , pattern AEInteger
  , pattern AEConstructor
  , pattern AEVariable
  , pattern AEVariableIdentifier
  , pattern AEApplication
  , pattern AEApplication2
  , pattern AEApplication3
  , pattern AELet
  , pattern AEMatch

  , AnnotatedExpressionL
  , MainAnnotatedExpressionL
  , pattern AnnotatedExpressionL
  , pattern AELInteger
  , pattern AELConstructor
  , pattern AELVariable
  , pattern AELVariableIdentifier
  , pattern AELApplication
  , pattern AELApplication2
  , pattern AELApplication3
  , pattern AELLet
  , pattern AELMatch
  , pattern AELLambda
  , pattern AELExpression

  , _annotation
  , _annotated
  ) where

import Control.Lens.TH
import Control.Lens.Tuple
import Control.Lens.Type
import Control.Lens.Wrapped ( _Wrapped )
import Data.Data
import Data.Kind ( Type )
import Data.Text.Prettyprint.Doc ( Pretty(..) )
import Data.Text.Prettyprint.Doc.Minicute
import GHC.Generics
import GHC.Show ( appPrec, appPrec1 )
import Language.Haskell.TH.Syntax ( Lift )
import Minicute.Data.Fix
import Minicute.Types.Minicute.Common
import Minicute.Types.Minicute.Expression
import Minicute.Types.Minicute.Precedence

import qualified Data.Text.Prettyprint.Doc as PP

-- |
-- @AnnotatedLetDefinition ann a@ is a 'LetDefinition' annotated with @ann@.
type AnnotatedLetDefinition ann a = LetDefinition_ (AnnotatedExpression ann) a
-- |
-- @MainAnnotatedLetDefinition ann@ is a 'MainLetDefinition' annotated with @ann@.
type MainAnnotatedLetDefinition ann = AnnotatedLetDefinition ann Identifier
-- |
-- Utility pattern for 'AnnotatedLetDefinition'
pattern AnnotatedLetDefinition :: a -> AnnotatedExpression ann a -> AnnotatedLetDefinition ann a
pattern AnnotatedLetDefinition a expr = LetDefinition_ (a, expr)
{-# COMPLETE AnnotatedLetDefinition #-}

-- |
-- @AnnotatedLetDefinitionL ann a@ is a 'LetDefinitionL' annotated with @ann@.
type AnnotatedLetDefinitionL ann a = LetDefinition_ (AnnotatedExpressionL ann) a
-- |
-- @MainAnnotatedLetDefinitionL ann@ is a 'MainLetDefinitionL' annotated with @ann@.
type MainAnnotatedLetDefinitionL ann = AnnotatedLetDefinitionL ann Identifier
-- |
-- Utility pattern for 'AnnotatedLetDefinitionL'
pattern AnnotatedLetDefinitionL :: a -> AnnotatedExpressionL ann a -> AnnotatedLetDefinitionL ann a
pattern AnnotatedLetDefinitionL a expr = LetDefinition_ (a, expr)
{-# COMPLETE AnnotatedLetDefinitionL #-}


-- |
-- @AnnotatedMatchCase ann a@ is a 'MatchCase' annotated with @ann@.
type AnnotatedMatchCase ann a = MatchCase_ (AnnotatedExpression ann) a
-- |
-- @MainAnnotatedMatchCase ann@ is a 'MainMatchCase' annotated with @ann@.
type MainAnnotatedMatchCase ann = AnnotatedMatchCase ann Identifier
-- |
-- Utility pattern for 'AnnotatedMatchCase'
pattern AnnotatedMatchCase :: Integer -> [a] -> AnnotatedExpression ann a -> AnnotatedMatchCase ann a
pattern AnnotatedMatchCase tag argBinders expr = MatchCase_ (tag, argBinders, expr)
{-# COMPLETE AnnotatedMatchCase #-}

-- |
-- @AnnotatedMatchCaseL ann a@ is a 'MatchCaseL' annotated with @ann@.
type AnnotatedMatchCaseL ann a = MatchCase_ (AnnotatedExpressionL ann) a
-- |
-- @MainAnnotatedMatchCaseL ann a@ is a 'MainMatchCaseL' annotated with @ann@.
type MainAnnotatedMatchCaseL ann = AnnotatedMatchCaseL ann Identifier
-- |
-- Utility pattern for 'AnnotatedMatchCaseL'
pattern AnnotatedMatchCaseL :: Integer -> [a] -> AnnotatedExpressionL ann a -> AnnotatedMatchCaseL ann a
pattern AnnotatedMatchCaseL tag argBinders expr = MatchCase_ (tag, argBinders, expr)
{-# COMPLETE AnnotatedMatchCaseL #-}


-- |
-- An internal type for an annotated expression.
--
-- [@ann@] a type for the annotation.
--
-- [@wExpr@] a structure of the expression ('Expression_' or 'ExpressionL_').
--
-- [@expr_@] a recursive part of the expression.
--
-- [@a@] an identifier type of the expression.
newtype AnnotatedExpression_ ann wExpr (expr_ :: Type -> Type) a
  = AnnotatedExpression_ (ann, wExpr expr_ a)
  deriving ( Generic
           , Typeable
           , Data
           , Lift
           , Eq
           , Ord
           , Show
           )

-- |
-- @AnnotatedExpression ann a@ is an 'Expression' annotated with @ann@.
type AnnotatedExpression ann = Fix2 (AnnotatedExpression_ ann Expression_)
-- |
-- @MainAnnotatedExpression ann@ is a 'MainExpression' annotated with @ann@.
type MainAnnotatedExpression ann = AnnotatedExpression ann Identifier

-- |
-- @AnnotatedExpression ann expr@ is @expr@ annotated with @ann@.
pattern AnnotatedExpression ann expr = AnnotatedExpressionFix2 (AnnotatedExpression_ (ann, expr))
{-# COMPLETE AnnotatedExpression #-}

-- |
-- Annotated 'EInteger'.
pattern AEInteger ann n = AnnotatedExpression ann (EInteger_ n)
-- |
-- Annotated 'EConstructor'.
pattern AEConstructor ann tag args = AnnotatedExpression ann (EConstructor_ tag args)
-- |
-- Annotated 'EVariable'.
pattern AEVariable ann v = AnnotatedExpression ann (EVariable_ v)
-- |
-- Annotated 'EVariableIdentifier'.
pattern AEVariableIdentifier ann v = AnnotatedExpression ann (EVariable_ (Identifier v))
-- |
-- Annotated 'EApplication'.
pattern AEApplication ann e1 e2 = AnnotatedExpression ann (EApplication_ e1 e2)
-- |
-- Annotated 'EApplication2'.
pattern AEApplication2 ann2 ann1 e1 e2 e3 = AEApplication ann2 (AEApplication ann1 e1 e2) e3
-- |
-- Annotated 'EApplication3'.
pattern AEApplication3 ann3 ann2 ann1 e1 e2 e3 e4 = AEApplication ann3 (AEApplication2 ann2 ann1 e1 e2 e3) e4
-- |
-- Annotated 'ELet'.
pattern AELet ann flag lds e = AnnotatedExpression ann (ELet_ flag lds e)
-- |
-- Annotated 'EMatch'.
pattern AEMatch ann e mcs = AnnotatedExpression ann (EMatch_ e mcs)
{-# COMPLETE AEInteger, AEConstructor, AEVariable, AEApplication, AELet, AEMatch #-}
{-# COMPLETE AEInteger, AEConstructor, AEVariableIdentifier, AEApplication, AELet, AEMatch #-}

pattern AnnotatedExpressionFix2 :: AnnotatedExpression_ ann Expression_ (AnnotatedExpression ann) a -> AnnotatedExpression ann a
pattern AnnotatedExpressionFix2 e = Fix2 e
{-# COMPLETE AnnotatedExpressionFix2 #-}

instance {-# OVERLAPS #-} (Show ann, Show a) => Show (AnnotatedExpression ann a) where
  showsPrec p (AEInteger ann n)
    = showParen (p > appPrec)
      $ showString "AEInteger " . showsPrec appPrec1 ann . showString " " . showsPrec appPrec1 n
  showsPrec p (AEConstructor ann tag args)
    = showParen (p > appPrec)
      $ showString "AEConstructor " . showsPrec appPrec1 ann . showString " " . showsPrec appPrec1 tag . showString " " . showsPrec appPrec1 args
  showsPrec p (AEVariable ann v)
    = showParen (p > appPrec)
      $ showString "AEVariable " . showsPrec appPrec1 ann . showString " " . showsPrec appPrec1 v
  showsPrec p (AEApplication ann e1 e2)
    = showParen (p > appPrec)
      $ showString "AEApplication " . showsPrec appPrec1 ann . showString " " . showsPrec appPrec1 e1 . showString " " . showsPrec appPrec1 e2
  showsPrec p (AELet ann flag lds e)
    = showParen (p > appPrec)
      $ showString "AELet " . showsPrec appPrec1 ann . showString " " . showsPrec appPrec1 flag . showString " " . showsPrec appPrec1 lds . showString " " . showsPrec appPrec1 e
  showsPrec p (AEMatch ann e mcs)
    = showParen (p > appPrec)
      $ showString "AEMatch " . showsPrec appPrec1 ann . showString " " . showsPrec appPrec1 e . showString " " . showsPrec appPrec1 mcs

instance (Pretty ann, Pretty a, Pretty (wExpr expr_ a)) => Pretty (AnnotatedExpression_ ann wExpr expr_ a) where
  pretty (AnnotatedExpression_ (ann, expr))
    = PP.parens (pretty ann PP.<> PP.comma PP.<+> pretty expr)

instance (Pretty ann, Pretty a, Pretty (wExpr expr_ a)) => PrettyPrec (AnnotatedExpression_ ann wExpr expr_ a)

instance (Pretty ann, Pretty a) => Pretty (AnnotatedExpression ann a) where
  pretty (AEApplication2 ann2 ann1 (AEVariableIdentifier annOp op) e1 e2)
    | Just opPrec <- lookup op binaryPrecedenceTable
    = PP.parens ((PP.hsep . PP.punctuate PP.comma . fmap pretty $ [ann2, ann1, annOp]) PP.<> PP.comma PP.<+> prettyBinaryExpressionPrec 0 op opPrec e1 e2)
  pretty expr = pretty (unFix2 expr)

instance (Pretty ann, Pretty a) => PrettyPrec (AnnotatedExpression ann a)

-- |
-- @AnnotatedExpressionL ann a@ is an 'ExpressionL' annotated with @ann@.
type AnnotatedExpressionL ann = Fix2 (AnnotatedExpression_ ann ExpressionL_)
-- |
-- @MainAnnotatedExpressionL ann@ is a 'MainExpressionL' annotated with @ann@.
type MainAnnotatedExpressionL ann = AnnotatedExpressionL ann Identifier

-- |
-- @AnnotatedExpressionL ann expr@ is @expr@ annotated by @ann@.
pattern AnnotatedExpressionL ann expr = AnnotatedExpressionLFix2 (AnnotatedExpression_ (ann, expr))
{-# COMPLETE AnnotatedExpressionL #-}

-- |
-- Annotated 'ELInteger'.
pattern AELInteger ann n = AELExpression ann (EInteger_ n)
-- |
-- Annotated 'ELConstructor'.
pattern AELConstructor ann tag args = AELExpression ann (EConstructor_ tag args)
-- |
-- Annotated 'ELVariable'.
pattern AELVariable ann v = AELExpression ann (EVariable_ v)
-- |
-- Annotated 'ELVariableIdentifier'.
pattern AELVariableIdentifier ann v = AELExpression ann (EVariable_ (Identifier v))
-- |
-- Annotated 'ELApplication'.
pattern AELApplication ann e1 e2 = AELExpression ann (EApplication_ e1 e2)
-- |
-- Annotated 'ELApplication2'.
pattern AELApplication2 ann2 ann1 e1 e2 e3 = AELApplication ann2 (AELApplication ann1 e1 e2) e3
-- |
-- Annotated 'ELApplication3'.
pattern AELApplication3 ann3 ann2 ann1 e1 e2 e3 e4 = AELApplication ann3 (AELApplication2 ann2 ann1 e1 e2 e3) e4
-- |
-- Annotated 'ELLet'.
pattern AELLet ann flag lds e = AELExpression ann (ELet_ flag lds e)
-- |
-- Annotated 'ELMatch'.
pattern AELMatch ann e mcs = AELExpression ann (EMatch_ e mcs)
-- |
-- Annotated 'ELLambda'.
pattern AELLambda ann as e = AnnotatedExpressionL ann (ELLambda_ as e)
{-# COMPLETE AELInteger, AELConstructor, AELVariable, AELApplication, AELLet, AELMatch, AELLambda #-}
{-# COMPLETE AELInteger, AELConstructor, AELVariableIdentifier, AELApplication, AELLet, AELMatch, AELLambda #-}

-- |
-- Annotated 'ELExpression'.
pattern AELExpression ann expr = AnnotatedExpressionL ann (ELExpression_ expr)
{-# COMPLETE AELExpression, AELLambda #-}

pattern AnnotatedExpressionLFix2 :: AnnotatedExpression_ ann ExpressionL_ (AnnotatedExpressionL ann) a -> AnnotatedExpressionL ann a
pattern AnnotatedExpressionLFix2 e = Fix2 e
{-# COMPLETE AnnotatedExpressionLFix2 #-}

instance {-# OVERLAPS #-} (Show ann, Show a) => Show (AnnotatedExpressionL ann a) where
  showsPrec p (AELInteger ann n)
    = showParen (p > appPrec)
      $ showString "AELInteger " . showsPrec appPrec1 ann . showString " " . showsPrec appPrec1 n
  showsPrec p (AELConstructor ann tag args)
    = showParen (p > appPrec)
      $ showString "AELConstructor " . showsPrec appPrec1 ann . showString " " . showsPrec appPrec1 tag . showString " " . showsPrec appPrec1 args
  showsPrec p (AELVariable ann v)
    = showParen (p > appPrec)
      $ showString "AELVariable " . showsPrec appPrec1 ann . showString " " . showsPrec appPrec1 v
  showsPrec p (AELApplication ann e1 e2)
    = showParen (p > appPrec)
      $ showString "AELApplication " . showsPrec appPrec1 ann . showString " " . showsPrec appPrec1 e1 . showString " " . showsPrec appPrec1 e2
  showsPrec p (AELLet ann flag lds e)
    = showParen (p > appPrec)
      $ showString "AELLet " . showsPrec appPrec1 ann . showString " " . showsPrec appPrec1 flag . showString " " . showsPrec appPrec1 lds . showString " " . showsPrec appPrec1 e
  showsPrec p (AELMatch ann e mcs)
    = showParen (p > appPrec)
      $ showString "AELMatch " . showsPrec appPrec1 ann . showString " " . showsPrec appPrec1 e . showString " " . showsPrec appPrec1 mcs
  showsPrec p (AELLambda ann as e)
    = showParen (p > appPrec)
      $ showString "AELLambda " . showsPrec appPrec1 ann . showString " " . showsPrec appPrec1 as . showString " " . showsPrec appPrec1 e

instance (Pretty ann, Pretty a) => Pretty (AnnotatedExpressionL ann a) where
  pretty (AELApplication2 ann2 ann1 (AELVariableIdentifier annOp op) e1 e2)
    | Just opPrec <- lookup op binaryPrecedenceTable
    = PP.parens ((PP.hsep . PP.punctuate PP.comma . fmap pretty $ [ann2, ann1, annOp]) PP.<> PP.comma PP.<+> prettyBinaryExpressionPrec 0 op opPrec e1 e2)
  pretty expr = pretty (unFix2 expr)

instance (Pretty ann, Pretty a) => PrettyPrec (AnnotatedExpressionL ann a)

makeWrapped ''AnnotatedExpression_

-- |
-- 'Lens' to extract the annotation of 'AnnotatedExpression_'.
_annotation :: Lens (AnnotatedExpression_ ann wExpr expr_ a) (AnnotatedExpression_ ann' wExpr expr_ a) ann ann'
_annotation = _Wrapped . _1
{-# INLINEABLE _annotation #-}

-- |
-- 'Lens' to extract the expression of 'AnnotatedExpression_'.
_annotated :: Lens (AnnotatedExpression_ ann wExpr expr_ a) (AnnotatedExpression_ ann wExpr' expr_' a') (wExpr expr_ a) (wExpr' expr_' a')
_annotated = _Wrapped . _2
{-# INLINEABLE _annotated #-}
