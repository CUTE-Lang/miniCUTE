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
-- Types for a miniCUTE expression
module Minicute.Types.Minicute.Expression
  ( module Minicute.Data.Fix
  , module Minicute.Types.Minicute.Common


  , LetDefinition_( .. )

  , LetDefinition
  , MainLetDefinition
  , pattern LetDefinition

  , LetDefinitionL
  , MainLetDefinitionL
  , pattern LetDefinitionL

  , _letDefinitionBinder
  , _letDefinitionBody


  , MatchCase_( .. )

  , MatchCase
  , MainMatchCase
  , pattern MatchCase

  , MatchCaseL
  , MainMatchCaseL
  , pattern MatchCaseL

  , _matchCaseTag
  , _matchCaseArguments
  , _matchCaseBody


  , Expression_( .. )

  , Expression
  , MainExpression
  , pattern EInteger
  , pattern EConstructor
  , pattern EVariable
  , pattern EVariableIdentifier
  , pattern EApplication
  , pattern EApplication2
  , pattern EApplication3
  , pattern ELet
  , pattern EMatch


  , ExpressionL_( .. )

  , ExpressionL
  , MainExpressionL
  , pattern ELInteger
  , pattern ELConstructor
  , pattern ELVariable
  , pattern ELVariableIdentifier
  , pattern ELApplication
  , pattern ELApplication2
  , pattern ELApplication3
  , pattern ELLet
  , pattern ELMatch
  , pattern ELLambda
  , pattern ELExpression
  ) where

import Control.Lens.TH
import Control.Lens.Tuple
import Control.Lens.Type
import Control.Lens.Wrapped ( _Wrapped )
import Data.Data
import Data.Text.Prettyprint.Doc ( Pretty(..) )
import Data.Text.Prettyprint.Doc.Minicute
import GHC.Generics
import GHC.Show ( appPrec, appPrec1 )
import Language.Haskell.TH.Syntax
import Minicute.Data.Fix
import Minicute.Types.Minicute.Common
import Minicute.Types.Minicute.Precedence

import qualified Data.Text.Prettyprint.Doc as PP

-- |
-- An internal type used to represent a let definition.
newtype LetDefinition_ expr_ a
  = LetDefinition_ (a, expr_ a)
  deriving ( Generic
           , Typeable
           , Data
           , Lift
           , Eq
           , Ord
           , Show
           )

-- |
-- 'LetDefinition_' for 'Expression'.
type LetDefinition a = LetDefinition_ Expression a
-- |
-- 'LetDefinition_' for 'MainExpression'.
type MainLetDefinition = LetDefinition Identifier
-- |
-- Utility pattern for 'LetDefinition'
pattern LetDefinition :: a -> Expression a -> LetDefinition a
pattern LetDefinition a expr = LetDefinition_ (a, expr)
{-# COMPLETE LetDefinition #-}

-- |
-- 'LetDefinition_' for 'ExpressionL'.
type LetDefinitionL a = LetDefinition_ ExpressionL a
-- |
-- 'LetDefinition_' for 'MainExpressionL'.
type MainLetDefinitionL = LetDefinitionL Identifier
-- |
-- Utility pattern for 'LetDefinitionL'
pattern LetDefinitionL :: a -> ExpressionL a -> LetDefinitionL a
pattern LetDefinitionL a expr = LetDefinition_ (a, expr)
{-# COMPLETE LetDefinitionL #-}

instance (Pretty a, Pretty (expr_ a)) => Pretty (LetDefinition_ expr_ a) where
  pretty (LetDefinition_ (binder, bodyExpr))
    = PP.hsep
      [ pretty binder
      , PP.equals
      , pretty bodyExpr
      ]


-- |
-- An internal type used to represent a match case.
newtype MatchCase_ expr_ a
  = MatchCase_ (Integer, [a], expr_ a)
  deriving ( Generic
           , Typeable
           , Data
           , Lift
           , Eq
           , Ord
           , Show
           )

-- |
-- 'MatchCase_' for 'Expression'.
type MatchCase a = MatchCase_ Expression a
-- |
-- 'MatchCase_' for 'MainExpression'.
type MainMatchCase = MatchCase Identifier
-- |
-- Utility pattern for 'MatchCase'
pattern MatchCase :: Integer -> [a] -> Expression a -> MatchCase a
pattern MatchCase tag argBinders expr = MatchCase_ (tag, argBinders, expr)
{-# COMPLETE MatchCase #-}

-- |
-- 'MatchCase_' for 'ExpressionL'.
type MatchCaseL a = MatchCase_ ExpressionL a
-- |
-- 'MatchCase_' for 'MainExpressionL'.
type MainMatchCaseL = MatchCaseL Identifier
-- |
-- Utility pattern for 'MatchCaseL'
pattern MatchCaseL :: Integer -> [a] -> ExpressionL a -> MatchCaseL a
pattern MatchCaseL tag argBinders expr = MatchCase_ (tag, argBinders, expr)
{-# COMPLETE MatchCaseL #-}

instance (Pretty a, Pretty (expr_ a)) => Pretty (MatchCase_ expr_ a) where
  pretty (MatchCase_ (tag, argBinders, bodyExpr))
    = PP.fuse PP.Shallow . PP.hcat
      $ [ PP.angles (pretty tag)
        , if null argBinders
          then PP.emptyDoc
          else PP.space
        , PP.hcat . PP.punctuate PP.space . fmap pretty $ argBinders
        , " -> "
        , pretty bodyExpr
        ]


-- |
-- An internal type for a miniCUTE expression.
--
-- This type requires usage of a type-level fixpoint.
data Expression_ expr_ a
  = EInteger_ Integer -- ^ @5@
  | EConstructor_ Integer Integer -- ^ @$C{t;a}@
  | EVariable_ Identifier -- ^ @v@
  | EApplication_ (expr_ a) (expr_ a) -- ^ @f 4@
  | ELet_ IsRecursive [LetDefinition_ expr_ a] (expr_ a) -- ^ @let x = 4 in x@
  | EMatch_ (expr_ a) [MatchCase_ expr_ a] -- ^ @match $C{1;0} with \<1\> -> 4@
  deriving ( Generic
           , Typeable
           , Data
           , Lift
           , Eq
           , Ord
           , Show
           )

-- |
-- A basic miniCUTE expression of @a@.
type Expression = Fix2 Expression_
-- |
-- A basic miniCUTE expression of @Identifier@.
type MainExpression = Expression Identifier
-- |
-- Utility pattern for Expression with 'EInteger_'.
pattern EInteger n = Fix2 (EInteger_ n)
-- |
-- Utility pattern for Expression with 'EConstructor_'.
pattern EConstructor tag args = Fix2 (EConstructor_ tag args)
-- |
-- Utility pattern for Expression with 'EVariable_'.
pattern EVariable v = Fix2 (EVariable_ v)
-- |
-- Utility pattern for Expression with 'EVariable_' and 'Identifier'.
pattern EVariableIdentifier v = Fix2 (EVariable_ (Identifier v))
-- |
-- Utility pattern for Expression with 'EApplication_'.
pattern EApplication e1 e2 = Fix2 (EApplication_ e1 e2)
-- |
-- Utility pattern for Expression with two consecutive 'EApplication_'s.
pattern EApplication2 e1 e2 e3 = EApplication (EApplication e1 e2) e3
-- |
-- Utility pattern for Expression with three consecutive 'EApplication_'s.
pattern EApplication3 e1 e2 e3 e4 = EApplication (EApplication2 e1 e2 e3) e4
-- |
-- Utility pattern for Expression with 'ELet_'.
pattern ELet flag lds e = Fix2 (ELet_ flag lds e)
-- |
-- Utility pattern for Expression with 'EMatch_'.
pattern EMatch e mcs = Fix2 (EMatch_ e mcs)
{-# COMPLETE EInteger, EConstructor, EVariable, EApplication, ELet, EMatch #-}
{-# COMPLETE EInteger, EConstructor, EVariableIdentifier, EApplication, ELet, EMatch #-}

instance {-# OVERLAPS #-} (Show a) => Show (Expression a) where
  showsPrec p (EInteger n)
    = showParen (p > appPrec)
      $ showString "EInteger " . showsPrec appPrec1 n
  showsPrec p (EConstructor tag args)
    = showParen (p > appPrec)
      $ showString "EConstructor " . showsPrec appPrec1 tag . showString " " . showsPrec appPrec1 args
  showsPrec p (EVariable v)
    = showParen (p > appPrec)
      $ showString "EVariable " . showsPrec appPrec1 v
  showsPrec p (EApplication e1 e2)
    = showParen (p > appPrec)
      $ showString "EApplication " . showsPrec appPrec1 e1 . showString " " . showsPrec appPrec1 e2
  showsPrec p (ELet flag lds e)
    = showParen (p > appPrec)
      $ showString "ELet " . showsPrec appPrec1 flag . showString " " . showsPrec appPrec1 lds . showString " " . showsPrec appPrec1 e
  showsPrec p (EMatch e mcs)
    = showParen (p > appPrec)
      $ showString "EMatch " . showsPrec appPrec1 e . showString " " . showsPrec appPrec1 mcs

instance (Pretty a, PrettyPrec (expr_ a)) => Pretty (Expression_ expr_ a) where
  pretty = prettyPrec0
  {-# INLINABLE pretty #-}

instance (Pretty a, PrettyPrec (expr_ a)) => PrettyPrec (Expression_ expr_ a) where
  prettyPrec _ (EInteger_ n) = pretty n
  prettyPrec _ (EConstructor_ tag arity)
    = PP.fuse PP.Shallow . PP.hcat
      $ [ "$C"
        , PP.braces . PP.hcat
          $ [ pretty tag
            , PP.comma
            , pretty arity
            ]
        ]
  prettyPrec _ (EVariable_ vId) = pretty vId
  prettyPrec p (EApplication_ e1 e2)
    = (if p > miniApplicationPrecedence then PP.parens else id) . PP.align . PP.hcat
      $ [ prettyPrec miniApplicationPrecedence e1
        , PP.space
        , prettyPrec miniApplicationPrecedence1 e2
        ]
  prettyPrec p (ELet_ flag letDefs e)
    = (if p > 0 then PP.parens else id) . PP.align . PP.hcat
      $ [ keyword
        , PP.line
        , prettyIndent . PP.vcat . PP.punctuate PP.semi . fmap pretty $ letDefs
        , PP.line
        , "in"
        , PP.line
        , prettyIndent . pretty $ e
        ]
    where
      keyword
        | isRecursive flag = "letrec"
        | otherwise = "let"
  prettyPrec p (EMatch_ e matchCases)
    = (if p > 0 then PP.parens else id) . PP.align . PP.hcat
      $ [ "match "
        , pretty e
        , " with"
        , PP.line
        , prettyIndent . PP.vcat . PP.punctuate PP.semi . fmap pretty $ matchCases
        ]

instance (Pretty a) => Pretty (Expression a) where
  pretty = prettyPrec0
  {-# INLINABLE pretty #-}

instance (Pretty a) => PrettyPrec (Expression a) where
  prettyPrec p (EApplication2 (EVariableIdentifier op) e1 e2)
    | Just opP <- lookup op binaryPrecedenceTable
    = prettyBinaryExpressionPrec p opP (pretty op) (`prettyPrec` e1) (`prettyPrec` e2)
  prettyPrec p expr = prettyPrec p (unFix2 expr)


-- |
-- An internal miniCUTE expression with __lambda__
data ExpressionL_ expr_ a
  = ELExpression_ (Expression_ expr_ a) -- ^ other 'Expression_'s
  | ELLambda_ [a] (expr_ a) -- ^ @\\x.x@
  deriving ( Generic
           , Typeable
           , Data
           , Lift
           , Eq
           , Ord
           , Show
           )

-- |
-- A lambda-containing miniCUTE expression of @a@.
type ExpressionL = Fix2 ExpressionL_
-- |
-- A lambda-containing miniCUTE expression of @Identifier@.
type MainExpressionL = ExpressionL Identifier
-- |
-- Utility pattern for ExpressionL of 'EInteger'.
pattern ELInteger n = ELExpression (EInteger_ n)
-- |
-- Utility pattern for ExpressionL of 'EConstructor'.
pattern ELConstructor tag args = ELExpression (EConstructor_ tag args)
-- |
-- Utility pattern for ExpressionL of 'EVariable'.
pattern ELVariable v = ELExpression (EVariable_ v)
-- |
-- Utility pattern for ExpressionL of 'EVariableIdentifier'.
pattern ELVariableIdentifier v = ELExpression (EVariable_ (Identifier v))
-- |
-- Utility pattern for ExpressionL of 'EApplication'.
pattern ELApplication e1 e2 = ELExpression (EApplication_ e1 e2)
-- |
-- Utility pattern for ExpressionL of 'EApplication2'.
pattern ELApplication2 e1 e2 e3 = ELApplication (ELApplication e1 e2) e3
-- |
-- Utility pattern for ExpressionL of 'EApplication3'.
pattern ELApplication3 e1 e2 e3 e4 = ELApplication (ELApplication2 e1 e2 e3) e4
-- |
-- Utility pattern for ExpressionL of 'ELet'.
pattern ELLet flag lds e = ELExpression (ELet_ flag lds e)
-- |
-- Utility pattern for ExpressionL of 'EMatch'.
pattern ELMatch e mcs = ELExpression (EMatch_ e mcs)
-- |
-- Utility pattern for ExpressionL with 'ELLambda_'.
pattern ELLambda as e = Fix2 (ELLambda_ as e)
{-# COMPLETE ELInteger, ELConstructor, ELVariable, ELApplication, ELLet, ELMatch, ELLambda #-}
{-# COMPLETE ELInteger, ELConstructor, ELVariableIdentifier, ELApplication, ELLet, ELMatch, ELLambda #-}
-- |
-- Utility pattern for ExpressionL with 'ELExpression_'.
pattern ELExpression e = Fix2 (ELExpression_ e)
{-# COMPLETE ELExpression, ELLambda #-}

instance {-# OVERLAPS #-} (Show a) => Show (ExpressionL a) where
  showsPrec p (ELInteger n)
    = showParen (p > appPrec)
      $ showString "ELInteger " . showsPrec appPrec1 n
  showsPrec p (ELConstructor tag args)
    = showParen (p > appPrec)
      $ showString "ELConstructor " . showsPrec appPrec1 tag . showString " " . showsPrec appPrec1 args
  showsPrec p (ELVariable v)
    = showParen (p > appPrec)
      $ showString "ELVariable " . showsPrec appPrec1 v
  showsPrec p (ELApplication e1 e2)
    = showParen (p > appPrec)
      $ showString "ELApplication " . showsPrec appPrec1 e1 . showString " " . showsPrec appPrec1 e2
  showsPrec p (ELLet flag lds e)
    = showParen (p > appPrec)
      $ showString "ELLet " . showsPrec appPrec1 flag . showString " " . showsPrec appPrec1 lds . showString " " . showsPrec appPrec1 e
  showsPrec p (ELMatch e mcs)
    = showParen (p > appPrec)
      $ showString "ELMatch " . showsPrec appPrec1 e . showString " " . showsPrec appPrec1 mcs
  showsPrec p (ELLambda as e)
    = showParen (p > appPrec)
      $ showString "ELLambda " . showsPrec appPrec1 as . showString " " . showsPrec appPrec1 e

instance (Pretty a, PrettyPrec (expr_ a)) => Pretty (ExpressionL_ expr_ a) where
  pretty = prettyPrec0
  {-# INLINABLE pretty #-}

instance (Pretty a, PrettyPrec (expr_ a)) => PrettyPrec (ExpressionL_ expr_ a) where
  prettyPrec p (ELExpression_ expr_) = prettyPrec p expr_
  prettyPrec p (ELLambda_ argBinders bodyExpr)
    = (if p > 0 then PP.parens else id) . PP.align . PP.hcat
      $ [ "\\"
        , PP.hcat . PP.punctuate PP.space . fmap pretty $ argBinders
        , " ->"
        , PP.line
        , prettyIndent . pretty $ bodyExpr
        ]

instance (Pretty a) => Pretty (ExpressionL a) where
  pretty = prettyPrec0
  {-# INLINABLE pretty #-}

instance (Pretty a) => PrettyPrec (ExpressionL a) where
  prettyPrec p (ELApplication2 (ELVariableIdentifier op) e1 e2)
    | Just opP <- lookup op binaryPrecedenceTable
    = prettyBinaryExpressionPrec p opP (pretty op) (`prettyPrec` e1) (`prettyPrec` e2)
  prettyPrec p expr = prettyPrec p (unFix2 expr)


-- |
-- @prettyIndent doc@ make a document indented with an appropriate size.
--
-- __TODO: extract this function into a separated module__
prettyIndent :: PP.Doc ann -> PP.Doc ann
prettyIndent = PP.indent 2
{-# INLINEABLE prettyIndent #-}


makeWrapped ''LetDefinition_

-- |
-- 'Lens' to extract the binder of 'LetDefinition_'
_letDefinitionBinder :: Lens' (LetDefinition_ expr_ a) a
_letDefinitionBinder = _Wrapped . _1
{-# INLINEABLE _letDefinitionBinder #-}

-- |
-- 'Lens' to extract the body expression of 'LetDefinition_'
_letDefinitionBody :: Lens (LetDefinition_ expr_ a) (LetDefinition_ expr_' a) (expr_ a) (expr_' a)
_letDefinitionBody = _Wrapped . _2
{-# INLINEABLE _letDefinitionBody #-}


makeWrapped ''MatchCase_

-- |
-- 'Lens' to extract the tag of 'MatchCase_'
_matchCaseTag :: Lens' (MatchCase_ expr_ a) Integer
_matchCaseTag = _Wrapped . _1
{-# INLINEABLE _matchCaseTag #-}

-- |
-- 'Lens' to extract the arguments of 'MatchCase_'
_matchCaseArguments :: Lens' (MatchCase_ expr_ a) [a]
_matchCaseArguments = _Wrapped . _2
{-# INLINEABLE _matchCaseArguments #-}

-- |
-- 'Lens' to extract the body expression of 'MatchCase_'
_matchCaseBody :: Lens (MatchCase_ expr_ a) (MatchCase_ expr_' a) (expr_ a) (expr_' a)
_matchCaseBody = _Wrapped . _3
{-# INLINEABLE _matchCaseBody #-}
