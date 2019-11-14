{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Types for a miniCUTE expression
module Minicute.Data.Minicute.Expression
  ( module Minicute.Data.Common


  , LetDefinition( .. )
  , LetDefinitionMC
  , LetDefinitionLLMC
  , MainLetDefinitionMC
  , MainLetDefinitionLLMC

  , _letDefinitionBinder
  , _letDefinitionBody


  , MatchCase( .. )
  , MatchCaseMC
  , MatchCaseLLMC
  , MainMatchCaseMC
  , MainMatchCaseLLMC

  , _matchCaseTag
  , _matchCaseArguments
  , _matchCaseBody


  , Expression( .. )
  , ExpressionMC
  , ExpressionLLMC
  , MainExpressionMC
  , MainExpressionLLMC
  , pattern EApplication2
  , pattern EApplication3
  ) where

import Control.Lens.TH
import Control.Lens.Tuple
import Control.Lens.Type
import Control.Lens.Wrapped ( _Wrapped )
import Data.Data
import Data.Text.Prettyprint.Doc.Minicute
import GHC.Generics
import Language.Haskell.TH.Syntax
import Minicute.Data.Common

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
-- A 'LetDefinition' for 'ExpressionMC'.
type LetDefinitionMC = LetDefinition ExpressionMC
-- |
-- A 'LetDefinition' for 'ExpressionLLMC'.
type LetDefinitionLLMC = LetDefinition ExpressionLLMC
-- |
-- A 'LetDefinition' for 'ExpressionMC' with 'Identifier'.
type MainLetDefinitionMC = LetDefinition ExpressionMC Identifier
-- |
-- A 'LetDefinition' for 'ExpressionLLMC' with 'Identifier'.
type MainLetDefinitionLLMC = LetDefinition ExpressionLLMC Identifier

instance (PrettyMC a, PrettyMC (expr a)) => PrettyMC (LetDefinition expr a) where
  prettyMC _ (LetDefinition (binder, expr))
    = PP.hsep
      [ prettyMC0 binder
      , PP.equals
      , prettyMC0 expr
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
-- A 'MatchCase' for 'ExpressionMC'.
type MatchCaseMC = MatchCase ExpressionMC
-- |
-- A 'MatchCase' for 'ExpressionLLMC'.
type MatchCaseLLMC = MatchCase ExpressionLLMC
-- |
-- A 'MatchCase' for 'ExpressionMC' with 'Identifier'.
type MainMatchCaseMC = MatchCase ExpressionMC Identifier
-- |
-- A 'MatchCase' for 'ExpressionLLMC' with 'Identifier'.
type MainMatchCaseLLMC = MatchCase ExpressionLLMC Identifier

instance (PrettyMC a, PrettyMC (expr a)) => PrettyMC (MatchCase expr a) where
  prettyMC _ (MatchCase (tag, argBinders, expr))
    = PP.fuse PP.Shallow . PP.hcat
      $ [ PP.angles (prettyMC0 tag)
        , if null argBinders
          then PP.emptyDoc
          else PP.space
        , PP.hcat . PP.punctuate PP.space . fmap prettyMC0 $ argBinders
        , " -> "
        , prettyMC0 expr
        ]


-- |
-- A basic miniCUTE expression of @a@.
data Expression (t :: ExpressionLevel) a where
  EInteger :: Integer -> Expression t a -- ^ @5@
  EConstructor :: Integer -> Integer -> Expression t a-- ^ @$C{t;a}@
  EVariable :: Identifier -> Expression t a-- ^ @v@
  EApplication :: Expression t a -> Expression t a -> Expression t a-- ^ @f 4@
  ELet :: IsRecursive -> [LetDefinition (Expression t) a] -> Expression t a -> Expression t a-- ^ @let x = 4 in x@
  EMatch :: Expression t a -> [MatchCase (Expression t) a] -> Expression t a-- ^ @match $C{1;0} with \<1\> -> 4@
  ELambda :: [a] -> ExpressionMC a -> ExpressionMC a -- ^ @\\x.x@
  deriving ( Typeable
           )
-- |
-- A 'Expression' of 'MC'
type ExpressionMC = Expression 'MC
-- |
-- A 'Expression' of 'MC'
type ExpressionLLMC = Expression 'LLMC
-- |
-- A 'Expression' of 'MC' with 'Identifier'.
type MainExpressionMC = ExpressionMC Identifier
-- |
-- A 'Expression' of 'LLMC' with 'Identifier'.
type MainExpressionLLMC = ExpressionLLMC Identifier

-- |
-- A utility pattern for 'Expression' of double application.
pattern EApplication2 e1 e2 e3 = EApplication (EApplication e1 e2) e3
-- |
-- A utility pattern for 'Expression' of triple application.
pattern EApplication3 e1 e2 e3 e4 = EApplication (EApplication2 e1 e2 e3) e4

deriving instance (Data a) => Data (ExpressionMC a)
deriving instance (Lift a) => Lift (Expression t a)
deriving instance (Eq a) => Eq (Expression t a)
deriving instance (Ord a) => Ord (Expression t a)
deriving instance (Show a) => Show (Expression t a)

instance (PrettyMC a) => PrettyMC (Expression t a) where
  prettyMC _ (EInteger n) = prettyMC0 n
  prettyMC _ (EConstructor tag arity)
    = PP.fuse PP.Shallow . PP.hcat
      $ [ "$C"
        , PP.braces . PP.hcat
          $ [ prettyMC0 tag
            , PP.comma
            , prettyMC0 arity
            ]
        ]
  prettyMC _ (EVariable vId) = prettyMC0 vId
  prettyMC p (EApplication2 (EVariable op) e1 e2)
    | Just opP <- lookup op binaryPrecedenceTable
    = prettyBinaryExpressionPrec p opP (prettyMC0 op) (`prettyMC` e1) (`prettyMC` e2)
  prettyMC p (EApplication e1 e2)
    = (if p > miniApplicationPrecedence then PP.parens else id) . PP.align . PP.hcat
      $ [ prettyMC miniApplicationPrecedence e1
        , PP.space
        , prettyMC miniApplicationPrecedence1 e2
        ]
  prettyMC p (ELet flag letDefs e)
    = (if p > 0 then PP.parens else id) . PP.align . PP.hcat
      $ [ keyword
        , PP.line
        , prettyIndent . PP.vcat . PP.punctuate PP.semi . fmap prettyMC0 $ letDefs
        , PP.line
        , "in"
        , PP.line
        , prettyIndent . prettyMC0 $ e
        ]
    where
      keyword
        | isRecursive flag = "letrec"
        | otherwise = "let"
  prettyMC p (EMatch e matchCases)
    = (if p > 0 then PP.parens else id) . PP.align . PP.hcat
      $ [ "match "
        , prettyMC0 e
        , " with"
        , PP.line
        , prettyIndent . PP.vcat . PP.punctuate PP.semi . fmap prettyMC0 $ matchCases
        ]
  prettyMC p (ELambda argBinders bodyExpr)
    = (if p > 0 then PP.parens else id) . PP.align . PP.hcat
      $ [ "\\"
        , PP.hcat . PP.punctuate PP.space . fmap prettyMC0 $ argBinders
        , " ->"
        , PP.line
        , prettyIndent . prettyMC0 $ bodyExpr
        ]


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
