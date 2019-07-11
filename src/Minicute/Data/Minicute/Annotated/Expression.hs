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


  , AnnotatedExpressionL( .. )
  , MainAnnotatedExpressionL
  , pattern AELApplication2
  , pattern AELApplication3

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
-- An internal type for an annotated expression.
--
-- [@ann@] a type for the annotation.
--
-- [@wExpr@] a structure of the expression ('Expression_' or 'ExpressionL_').
--
-- [@expr_@] a recursive part of the expression.
--
-- [@a@] an identifier type of the expression.
data AnnotatedExpressionL ann a
  = AELInteger ann Integer -- ^ @5@
  | AELConstructor ann Integer Integer -- ^ @$C{t;a}@
  | AELVariable ann Identifier -- ^ @v@
  | AELApplication ann (AnnotatedExpressionL ann a) (AnnotatedExpressionL ann a) -- ^ @f 4@
  | AELLet ann IsRecursive [LetDefinition (AnnotatedExpressionL ann) a] (AnnotatedExpressionL ann a) -- ^ @let x = 4 in x@
  | AELMatch ann (AnnotatedExpressionL ann a) [MatchCase (AnnotatedExpressionL ann) a] -- ^ @match $C{1;0} with \<1\> -> 4@
  | AELLambda ann [a] (AnnotatedExpressionL ann a) -- ^ @\\x.x@
  deriving ( Generic
           , Typeable
           , Data
           , Lift
           , Eq
           , Ord
           , Show
           )
-- |
-- @MainAnnotatedExpressionL ann@ is a 'MainExpressionL' annotated with @ann@.
type MainAnnotatedExpressionL ann = AnnotatedExpressionL ann Identifier

-- |
-- Annotated 'ELApplication2'.
pattern AELApplication2 ann2 ann1 e1 e2 e3 = AELApplication ann2 (AELApplication ann1 e1 e2) e3
-- |
-- Annotated 'ELApplication3'.
pattern AELApplication3 ann3 ann2 ann1 e1 e2 e3 e4 = AELApplication ann3 (AELApplication2 ann2 ann1 e1 e2 e3) e4

instance (Pretty ann, Pretty a) => Pretty (AnnotatedExpressionL ann a) where
  pretty = prettyPrec0

instance (Pretty ann, Pretty a) => PrettyPrec (AnnotatedExpressionL ann a) where
  prettyPrec _ (AELInteger ann n) = pretty n PP.<> PP.braces (pretty ann)
  prettyPrec _ (AELConstructor ann tag arity)
    = ( PP.fuse PP.Shallow . PP.hcat
        $ [ "$C"
          , PP.braces . PP.hcat
            $ [ pretty tag
              , PP.comma
              , pretty arity
              ]
          ]
      ) PP.<> PP.braces (pretty ann)
  prettyPrec _ (AELVariable ann vId) = pretty vId PP.<> PP.braces (pretty ann)
  prettyPrec _ (AELApplication2 ann2 ann1 (AELVariable annOp (Identifier op)) e1 e2)
    | Just opP <- lookup op binaryPrecedenceTable
    = prettyBinaryExpressionPrec miniApplicationPrecedence1 opP opDoc (`prettyPrec` e1) (`prettyPrec` e2)
      PP.<> PP.braces (pretty ann1 PP.<> PP.comma PP.<+> pretty ann2)
    where
      opDoc = pretty op PP.<> PP.braces (pretty annOp)
  prettyPrec p (AELApplication ann e1 e2)
    = (if p > miniApplicationPrecedence then PP.parens else id)
      $ ( PP.align . PP.hcat
          $ [ prettyPrec miniApplicationPrecedence e1
            , PP.space
            , prettyPrec miniApplicationPrecedence1 e2
            ]
        ) PP.<> PP.braces (pretty ann)
  prettyPrec p (AELLet ann flag letDefs e)
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
  prettyPrec p (AELMatch ann e matchCases)
    = (if p > 0 then PP.parens else id)
      $ ( PP.align . PP.hcat
          $ [ "match "
            , pretty e
            , " with"
            , PP.line
            , prettyIndent . PP.vcat . PP.punctuate PP.semi . fmap pretty $ matchCases
            ]
        ) PP.<> PP.braces (pretty ann)
  prettyPrec p (AELLambda ann argBinders bodyExpr)
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
-- 'Lens' to extract the annotation of 'AnnotatedExpressionL'.
_annotation :: Lens' (AnnotatedExpressionL ann a) ann
_annotation = lens getter setter
  where
    getter (AELInteger ann _) = ann
    getter (AELConstructor ann _ _) = ann
    getter (AELVariable ann _) = ann
    getter (AELApplication ann _ _) = ann
    getter (AELLet ann _ _ _) = ann
    getter (AELMatch ann _ _) = ann
    getter (AELLambda ann _ _) = ann

    setter (AELInteger _ n) ann = AELInteger ann n
    setter (AELConstructor _ t a) ann = AELConstructor ann t a
    setter (AELVariable _ v) ann = AELVariable ann v
    setter (AELApplication _ e1 e2) ann = AELApplication ann e1 e2
    setter (AELLet _ flag lDefs expr) ann = AELLet ann flag lDefs expr
    setter (AELMatch _ mCases expr) ann = AELMatch ann mCases expr
    setter (AELLambda _ argBinders expr) ann = AELLambda ann argBinders expr
{-# INLINEABLE _annotation #-}
