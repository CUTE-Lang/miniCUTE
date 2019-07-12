{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
-- |
-- Types for annotated expressions
module Minicute.Data.Minicute.Annotated.Expression
  ( module Minicute.Data.Fix
  , module Minicute.Data.Minicute.Common
  , module Minicute.Data.Minicute.Expression


  , AnnotatedExpressionMC( .. )
  , MainAnnotatedExpressionMC
  , pattern AEApplication2
  , pattern AEApplication3

  , _annotation
  ) where

import Control.Lens.Lens ( lens )
import Control.Lens.Type
import Data.Data
import Data.Text.Prettyprint.Doc ( Pretty(..) )
import Data.Text.Prettyprint.Doc.Minicute
import GHC.Generics
import Language.Haskell.TH.Syntax ( Lift )
import Minicute.Data.Fix
import Minicute.Data.Minicute.Common
import Minicute.Data.Minicute.Expression
import Minicute.Data.Minicute.Precedence

import qualified Data.Text.Prettyprint.Doc as PP


-- |
-- A type for an annotated expression.
--
-- [@ann@] a type for the annotation.
--
-- [@a@] an identifier type of the expression.
data AnnotatedExpressionMC ann a
  = AEInteger ann Integer -- ^ @5@
  | AEConstructor ann Integer Integer -- ^ @$C{t;a}@
  | AEVariable ann Identifier -- ^ @v@
  | AEApplication ann (AnnotatedExpressionMC ann a) (AnnotatedExpressionMC ann a) -- ^ @f 4@
  | AELet ann IsRecursive [LetDefinition (AnnotatedExpressionMC ann) a] (AnnotatedExpressionMC ann a) -- ^ @let x = 4 in x@
  | AEMatch ann (AnnotatedExpressionMC ann a) [MatchCase (AnnotatedExpressionMC ann) a] -- ^ @match $C{1;0} with \<1\> -> 4@
  | AELambda ann [a] (AnnotatedExpressionMC ann a) -- ^ @\\x.x@
  deriving ( Generic
           , Typeable
           , Data
           , Lift
           , Eq
           , Ord
           , Show
           )
-- |
-- @MainAnnotatedExpressionMC ann@ is a 'MainExpressionMC' annotated with @ann@.
type MainAnnotatedExpressionMC ann = AnnotatedExpressionMC ann Identifier

-- |
-- Annotated 'EApplication2'.
pattern AEApplication2 ann2 ann1 e1 e2 e3 = AEApplication ann2 (AEApplication ann1 e1 e2) e3
-- |
-- Annotated 'EApplication3'.
pattern AEApplication3 ann3 ann2 ann1 e1 e2 e3 e4 = AEApplication ann3 (AEApplication2 ann2 ann1 e1 e2 e3) e4

instance (Pretty ann, Pretty a) => Pretty (AnnotatedExpressionMC ann a) where
  pretty = prettyPrec0

instance (Pretty ann, Pretty a) => PrettyPrec (AnnotatedExpressionMC ann a) where
  prettyPrec _ (AEInteger ann n) = pretty n PP.<> PP.braces (pretty ann)
  prettyPrec _ (AEConstructor ann tag arity)
    = ( PP.fuse PP.Shallow . PP.hcat
        $ [ "$C"
          , PP.braces . PP.hcat
            $ [ pretty tag
              , PP.comma
              , pretty arity
              ]
          ]
      ) PP.<> PP.braces (pretty ann)
  prettyPrec _ (AEVariable ann vId) = pretty vId PP.<> PP.braces (pretty ann)
  prettyPrec _ (AEApplication2 ann2 ann1 (AEVariable annOp (Identifier op)) e1 e2)
    | Just opP <- lookup op binaryPrecedenceTable
    = prettyBinaryExpressionPrec miniApplicationPrecedence1 opP opDoc (`prettyPrec` e1) (`prettyPrec` e2)
      PP.<> PP.braces (pretty ann1 PP.<> PP.comma PP.<+> pretty ann2)
    where
      opDoc = pretty op PP.<> PP.braces (pretty annOp)
  prettyPrec p (AEApplication ann e1 e2)
    = (if p > miniApplicationPrecedence then PP.parens else id)
      $ ( PP.align . PP.hcat
          $ [ prettyPrec miniApplicationPrecedence e1
            , PP.space
            , prettyPrec miniApplicationPrecedence1 e2
            ]
        ) PP.<> PP.braces (pretty ann)
  prettyPrec p (AELet ann flag letDefs e)
    = (if p > 0 then PP.parens else id)
      $ ( PP.align . PP.hcat
          $ [ keyword
            , PP.line
            , prettyIndent . PP.vcat . PP.punctuate PP.semi . fmap pretty $ letDefs
            , PP.line
            , "in"
            , PP.line
            , prettyIndent . pretty $ e
            ]
        ) PP.<> PP.braces (pretty ann)
    where
      keyword
        | isRecursive flag = "letrec"
        | otherwise = "let"
  prettyPrec p (AEMatch ann e matchCases)
    = (if p > 0 then PP.parens else id)
      $ ( PP.align . PP.hcat
          $ [ "match "
            , pretty e
            , " with"
            , PP.line
            , prettyIndent . PP.vcat . PP.punctuate PP.semi . fmap pretty $ matchCases
            ]
        ) PP.<> PP.braces (pretty ann)
  prettyPrec p (AELambda ann argBinders bodyExpr)
    = (if p > 0 then PP.parens else id)
      $ ( PP.align . PP.hcat
          $ [ "\\"
            , PP.hcat . PP.punctuate PP.space . fmap pretty $ argBinders
            , " ->"
            , PP.line
            , prettyIndent . pretty $ bodyExpr
            ]
        ) PP.<> PP.braces (pretty ann)


-- |
-- 'Lens' to extract the annotation of 'AnnotatedExpressionMC'.
_annotation :: Lens' (AnnotatedExpressionMC ann a) ann
_annotation = lens getter setter
  where
    getter (AEInteger ann _) = ann
    getter (AEConstructor ann _ _) = ann
    getter (AEVariable ann _) = ann
    getter (AEApplication ann _ _) = ann
    getter (AELet ann _ _ _) = ann
    getter (AEMatch ann _ _) = ann
    getter (AELambda ann _ _) = ann

    setter (AEInteger _ n) ann = AEInteger ann n
    setter (AEConstructor _ t a) ann = AEConstructor ann t a
    setter (AEVariable _ v) ann = AEVariable ann v
    setter (AEApplication _ e1 e2) ann = AEApplication ann e1 e2
    setter (AELet _ flag lDefs expr) ann = AELet ann flag lDefs expr
    setter (AEMatch _ mCases expr) ann = AEMatch ann mCases expr
    setter (AELambda _ argBinders expr) ann = AELambda ann argBinders expr
{-# INLINEABLE _annotation #-}
