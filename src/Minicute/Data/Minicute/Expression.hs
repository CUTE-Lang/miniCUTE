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
module Minicute.Data.Minicute.Expression
  ( module Minicute.Data.Minicute.Common


  , LetDefinition( .. )
  , MainLetDefinition
  , MainLetDefinitionL

  , _letDefinitionBinder
  , _letDefinitionBody


  , MatchCase( .. )
  , MainMatchCase
  , MainMatchCaseL

  , _matchCaseTag
  , _matchCaseArguments
  , _matchCaseBody


  , Expression( .. )
  , MainExpression
  , pattern EApplication2
  , pattern EApplication3


  , ExpressionL( .. )
  , MainExpressionL
  , pattern ELApplication2
  , pattern ELApplication3

  -- __TODO: remove this__
  , prettyIndent
  ) where

import Control.Lens.TH
import Control.Lens.Tuple
import Control.Lens.Type
import Control.Lens.Wrapped ( _Wrapped )
import Data.Data
import Data.Text.Prettyprint.Doc ( Pretty(..) )
import Data.Text.Prettyprint.Doc.Minicute
import GHC.Generics
import Language.Haskell.TH.Syntax
import Minicute.Data.Minicute.Common
import Minicute.Data.Minicute.Precedence

import qualified Data.Text.Prettyprint.Doc as PP

-- |
-- A type used to represent a let definition of @expr a@.
newtype LetDefinition expr a
  = LetDefinition (a, expr a)
  deriving ( Generic
           , Typeable
           , Data
           , Lift
           , Eq
           , Ord
           , Show
           )
-- |
-- A let definition for 'MainExpression'.
type MainLetDefinition = LetDefinition Expression Identifier
-- |
-- A let definition for 'MainExpressionL'.
type MainLetDefinitionL = LetDefinition ExpressionL Identifier

instance (Pretty a, Pretty (expr a)) => Pretty (LetDefinition expr a) where
  pretty (LetDefinition (binder, expr))
    = PP.hsep
      [ pretty binder
      , PP.equals
      , pretty expr
      ]


-- |
-- A type used to represent a match case of @expr a@.
newtype MatchCase expr a
  = MatchCase (Integer, [a], expr a)
  deriving ( Generic
           , Typeable
           , Data
           , Lift
           , Eq
           , Ord
           , Show
           )
-- |
-- A match case for 'MainExpression'.
type MainMatchCase = MatchCase Expression Identifier
-- |
-- A match case for 'MainExpressionL'.
type MainMatchCaseL = MatchCase ExpressionL Identifier

instance (Pretty a, Pretty (expr a)) => Pretty (MatchCase expr a) where
  pretty (MatchCase (tag, argBinders, expr))
    = PP.fuse PP.Shallow . PP.hcat
      $ [ PP.angles (pretty tag)
        , if null argBinders
          then PP.emptyDoc
          else PP.space
        , PP.hcat . PP.punctuate PP.space . fmap pretty $ argBinders
        , " -> "
        , pretty expr
        ]


-- |
-- A basic miniCUTE expression of @a@.
data Expression a
  = EInteger Integer -- ^ @5@
  | EConstructor Integer Integer -- ^ @$C{t;a}@
  | EVariable Identifier -- ^ @v@
  | EApplication (Expression a) (Expression a) -- ^ @f 4@
  | ELet IsRecursive [LetDefinition Expression a] (Expression a) -- ^ @let x = 4 in x@
  | EMatch (Expression a) [MatchCase Expression a] -- ^ @match $C{1;0} with \<1\> -> 4@
  deriving ( Generic
           , Typeable
           , Data
           , Lift
           , Eq
           , Ord
           , Show
           )
-- |
-- A basic miniCUTE expression of @Identifier@.
type MainExpression = Expression Identifier

-- |
-- A utility pattern for 'Expression' of double application.
pattern EApplication2 e1 e2 e3 = EApplication (EApplication e1 e2) e3
-- |
-- A utility pattern for 'Expression' of triple application.
pattern EApplication3 e1 e2 e3 e4 = EApplication (EApplication2 e1 e2 e3) e4

instance (Pretty a) => Pretty (Expression a) where
  pretty = prettyPrec0
  {-# INLINABLE pretty #-}

instance (Pretty a) => PrettyPrec (Expression a) where
  prettyPrec _ (EInteger n) = pretty n
  prettyPrec _ (EConstructor tag arity)
    = PP.fuse PP.Shallow . PP.hcat
      $ [ "$C"
        , PP.braces . PP.hcat
          $ [ pretty tag
            , PP.comma
            , pretty arity
            ]
        ]
  prettyPrec _ (EVariable vId) = pretty vId
  prettyPrec p (EApplication2 (EVariable (Identifier op)) e1 e2)
    | Just opP <- lookup op binaryPrecedenceTable
    = prettyBinaryExpressionPrec p opP (pretty op) (`prettyPrec` e1) (`prettyPrec` e2)
  prettyPrec p (EApplication e1 e2)
    = (if p > miniApplicationPrecedence then PP.parens else id) . PP.align . PP.hcat
      $ [ prettyPrec miniApplicationPrecedence e1
        , PP.space
        , prettyPrec miniApplicationPrecedence1 e2
        ]
  prettyPrec p (ELet flag letDefs e)
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
  prettyPrec p (EMatch e matchCases)
    = (if p > 0 then PP.parens else id) . PP.align . PP.hcat
      $ [ "match "
        , pretty e
        , " with"
        , PP.line
        , prettyIndent . PP.vcat . PP.punctuate PP.semi . fmap pretty $ matchCases
        ]


-- |
-- A lambda-containing miniCUTE expression of @a@.
data ExpressionL a
  = ELInteger Integer -- ^ @5@
  | ELConstructor Integer Integer -- ^ @$C{t;a}@
  | ELVariable Identifier -- ^ @v@
  | ELApplication (ExpressionL a) (ExpressionL a) -- ^ @f 4@
  | ELLet IsRecursive [LetDefinition ExpressionL a] (ExpressionL a) -- ^ @let x = 4 in x@
  | ELMatch (ExpressionL a) [MatchCase ExpressionL a] -- ^ @match $C{1;0} with \<1\> -> 4@
  | ELLambda [a] (ExpressionL a) -- ^ @\\x.x@
  deriving ( Generic
           , Typeable
           , Data
           , Lift
           , Eq
           , Ord
           , Show
           )
-- |
-- A lambda-containing miniCUTE expression of 'Identifier'.
type MainExpressionL = ExpressionL Identifier
-- |
-- A utility pattern for 'ExpressionL' of double application.
pattern ELApplication2 e1 e2 e3 = ELApplication (ELApplication e1 e2) e3
-- |
-- A utility pattern for 'ExpressionL' of triple application.
pattern ELApplication3 e1 e2 e3 e4 = ELApplication (ELApplication2 e1 e2 e3) e4

instance (Pretty a) => Pretty (ExpressionL a) where
  pretty = prettyPrec0
  {-# INLINABLE pretty #-}

instance (Pretty a) => PrettyPrec (ExpressionL a) where
  prettyPrec _ (ELInteger n) = pretty n
  prettyPrec _ (ELConstructor tag arity)
    = PP.fuse PP.Shallow . PP.hcat
      $ [ "$C"
        , PP.braces . PP.hcat
          $ [ pretty tag
            , PP.comma
            , pretty arity
            ]
        ]
  prettyPrec _ (ELVariable vId) = pretty vId
  prettyPrec p (ELApplication2 (ELVariable (Identifier op)) e1 e2)
    | Just opP <- lookup op binaryPrecedenceTable
    = prettyBinaryExpressionPrec p opP (pretty op) (`prettyPrec` e1) (`prettyPrec` e2)
  prettyPrec p (ELApplication e1 e2)
    = (if p > miniApplicationPrecedence then PP.parens else id) . PP.align . PP.hcat
      $ [ prettyPrec miniApplicationPrecedence e1
        , PP.space
        , prettyPrec miniApplicationPrecedence1 e2
        ]
  prettyPrec p (ELLet flag letDefs e)
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
  prettyPrec p (ELMatch e matchCases)
    = (if p > 0 then PP.parens else id) . PP.align . PP.hcat
      $ [ "match "
        , pretty e
        , " with"
        , PP.line
        , prettyIndent . PP.vcat . PP.punctuate PP.semi . fmap pretty $ matchCases
        ]
  prettyPrec p (ELLambda argBinders bodyExpr)
    = (if p > 0 then PP.parens else id) . PP.align . PP.hcat
      $ [ "\\"
        , PP.hcat . PP.punctuate PP.space . fmap pretty $ argBinders
        , " ->"
        , PP.line
        , prettyIndent . pretty $ bodyExpr
        ]


-- |
-- @prettyIndent doc@ make a document indented with an appropriate size.
--
-- __TODO: extract this function into a separated module__
prettyIndent :: PP.Doc ann -> PP.Doc ann
prettyIndent = PP.indent 2
{-# INLINEABLE prettyIndent #-}


makeWrapped ''LetDefinition

-- |
-- 'Lens' to extract the binder of 'LetDefinition'
_letDefinitionBinder :: Lens' (LetDefinition expr a) a
_letDefinitionBinder = _Wrapped . _1
{-# INLINEABLE _letDefinitionBinder #-}

-- |
-- 'Lens' to extract the body expression of 'LetDefinition'
_letDefinitionBody :: Lens (LetDefinition expr a) (LetDefinition expr' a) (expr a) (expr' a)
_letDefinitionBody = _Wrapped . _2
{-# INLINEABLE _letDefinitionBody #-}


makeWrapped ''MatchCase

-- |
-- 'Lens' to extract the tag of 'MatchCase'
_matchCaseTag :: Lens' (MatchCase expr a) Integer
_matchCaseTag = _Wrapped . _1
{-# INLINEABLE _matchCaseTag #-}

-- |
-- 'Lens' to extract the arguments of 'MatchCase'
_matchCaseArguments :: Lens' (MatchCase expr a) [a]
_matchCaseArguments = _Wrapped . _2
{-# INLINEABLE _matchCaseArguments #-}

-- |
-- 'Lens' to extract the body expression of 'MatchCase'
_matchCaseBody :: Lens (MatchCase expr a) (MatchCase expr' a) (expr a) (expr' a)
_matchCaseBody = _Wrapped . _3
{-# INLINEABLE _matchCaseBody #-}
