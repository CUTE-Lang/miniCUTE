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

type LetDefinition a = LetDefinition_ Expression a
type MainLetDefinition = LetDefinition Identifier
pattern LetDefinition :: a -> Expression a -> LetDefinition a
pattern LetDefinition a expr = LetDefinition_ (a, expr)
{-# COMPLETE LetDefinition #-}

type LetDefinitionL a = LetDefinition_ ExpressionL a
type MainLetDefinitionL = LetDefinitionL Identifier
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

type MatchCase a = MatchCase_ Expression a
type MainMatchCase = MatchCase Identifier
pattern MatchCase :: Integer -> [a] -> Expression a -> MatchCase a
pattern MatchCase tag argBinders expr = MatchCase_ (tag, argBinders, expr)
{-# COMPLETE MatchCase #-}

type MatchCaseL a = MatchCase_ ExpressionL a
type MainMatchCaseL = MatchCaseL Identifier
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


data Expression_ expr_ a
  = EInteger_ Integer
  | EConstructor_ Integer Integer
  | EVariable_ Identifier
  | EApplication_ (expr_ a) (expr_ a)
  | ELet_ IsRecursive [LetDefinition_ expr_ a] (expr_ a)
  | EMatch_ (expr_ a) [MatchCase_ expr_ a]
  deriving ( Generic
           , Typeable
           , Data
           , Lift
           , Eq
           , Ord
           , Show
           )

type Expression = Fix2 Expression_
type MainExpression = Expression Identifier
pattern EInteger n = Fix2 (EInteger_ n)
pattern EConstructor tag args = Fix2 (EConstructor_ tag args)
pattern EVariable v = Fix2 (EVariable_ v)
pattern EVariableIdentifier v = Fix2 (EVariable_ (Identifier v))
pattern EApplication e1 e2 = Fix2 (EApplication_ e1 e2)
pattern EApplication2 e1 e2 e3 = EApplication (EApplication e1 e2) e3
pattern EApplication3 e1 e2 e3 e4 = EApplication (EApplication2 e1 e2 e3) e4
pattern ELet flag lds e = Fix2 (ELet_ flag lds e)
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
    | Just opPrec <- lookup op binaryPrecedenceTable
    = prettyBinaryExpressionPrec p op opPrec e1 e2
  prettyPrec p expr = prettyPrec p (unFix2 expr)


data ExpressionL_ expr_ a
  = ELExpression_ (Expression_ expr_ a)
  | ELLambda_ [a] (expr_ a)
  deriving ( Generic
           , Typeable
           , Data
           , Lift
           , Eq
           , Ord
           , Show
           )

type ExpressionL = Fix2 ExpressionL_
type MainExpressionL = ExpressionL Identifier
pattern ELInteger n = ELExpression (EInteger_ n)
pattern ELConstructor tag args = ELExpression (EConstructor_ tag args)
pattern ELVariable v = ELExpression (EVariable_ v)
pattern ELVariableIdentifier v = ELExpression (EVariable_ (Identifier v))
pattern ELApplication e1 e2 = ELExpression (EApplication_ e1 e2)
pattern ELApplication2 e1 e2 e3 = ELApplication (ELApplication e1 e2) e3
pattern ELApplication3 e1 e2 e3 e4 = ELApplication (ELApplication2 e1 e2 e3) e4
pattern ELLet flag lds e = ELExpression (ELet_ flag lds e)
pattern ELMatch e mcs = ELExpression (EMatch_ e mcs)
pattern ELLambda as e = Fix2 (ELLambda_ as e)
{-# COMPLETE ELInteger, ELConstructor, ELVariable, ELApplication, ELLet, ELMatch, ELLambda #-}
{-# COMPLETE ELInteger, ELConstructor, ELVariableIdentifier, ELApplication, ELLet, ELMatch, ELLambda #-}
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
    | Just opPrec <- lookup op binaryPrecedenceTable
    = prettyBinaryExpressionPrec p op opPrec e1 e2
  prettyPrec p expr = prettyPrec p (unFix2 expr)


prettyIndent :: PP.Doc ann -> PP.Doc ann
prettyIndent = PP.indent 2
{-# INLINEABLE prettyIndent #-}


makeWrapped ''LetDefinition_

_letDefinitionBinder :: Lens' (LetDefinition_ expr_ a) a
_letDefinitionBinder = _Wrapped . _1
{-# INLINEABLE _letDefinitionBinder #-}

_letDefinitionBody :: Lens (LetDefinition_ expr_ a) (LetDefinition_ expr_' a) (expr_ a) (expr_' a)
_letDefinitionBody = _Wrapped . _2
{-# INLINEABLE _letDefinitionBody #-}


makeWrapped ''MatchCase_

_matchCaseTag :: Lens' (MatchCase_ expr_ a) Integer
_matchCaseTag = _Wrapped . _1
{-# INLINEABLE _matchCaseTag #-}

_matchCaseArguments :: Lens' (MatchCase_ expr_ a) [a]
_matchCaseArguments = _Wrapped . _2
{-# INLINEABLE _matchCaseArguments #-}

_matchCaseBody :: Lens (MatchCase_ expr_ a) (MatchCase_ expr_' a) (expr_ a) (expr_' a)
_matchCaseBody = _Wrapped . _3
{-# INLINEABLE _matchCaseBody #-}
