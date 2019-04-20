{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Minicute.PrettyPrintable
  ( PrettyPrintable( .. )
  ) where

import Minicute.Data.Fix
import Minicute.Data.PrintSequence
import Minicute.Types.Minicute.Expression
import Minicute.Types.Minicute.Program

class PrettyPrintable a where
  prettyPrint :: a -> PrintSequence

prettyPrintList :: (PrettyPrintable a) => PrintSequence -> [a] -> PrintSequence
prettyPrintList sep = printInterleave sep . fmap prettyPrint

instance (PrettyPrintable a, PrettyPrintable expr) => PrettyPrintable (Program# a expr) where
  prettyPrint (Program# scs) = prettyPrintList printNewline scs

instance (PrettyPrintable a, PrettyPrintable expr) => PrettyPrintable (Supercombinator# a expr) where
  prettyPrint (scId, argBinders, expr)
    = printConcat
      [ prettyPrint scId
      , printString " "
      , prettyPrintList prettyPrintSpace argBinders
      , printString " = "
      , prettyPrint expr
      ]

instance (PrettyPrintable a) => PrettyPrintable (ExpressionL a) where
  prettyPrint (Fix2 expr#) = prettyPrint expr#

instance (PrettyPrintable a, PrettyPrintable (expr_ a)) => PrettyPrintable (ExpressionL# expr_ a) where
  prettyPrint (ELExpression# expr#) = prettyPrint expr#
  prettyPrint (ELLambda# argBinders bodyExpr)
    = printConcat
      [ printString "\\"
      , prettyPrintList prettyPrintSpace argBinders
      , prettyPrint bodyExpr
      ]

instance (PrettyPrintable a) => PrettyPrintable (Expression a) where
  prettyPrint (Fix2 expr#) = prettyPrint expr#

instance (PrettyPrintable a, PrettyPrintable (expr_ a)) => PrettyPrintable (Expression# expr_ a) where
  prettyPrint (EInteger# int#) = printIntegral int#
  prettyPrint (EConstructor# tag arity)
    = printConcat
      [ printString "$C{"
      , printIntegral tag
      , printString ","
      , printIntegral arity
      , printString "}"
      ]
  prettyPrint (EVariable# vId) = prettyPrint vId
  prettyPrint (EApplication# e1 e2)
    = printConcat
      [ prettyPrint e1
      , printString " "
      , prettyPrint e2
      ]
  prettyPrint (ELet# flag letDefs e)
    = printConcat
      [ printString keyword
      , printString " "
      , prettyPrintList prettyPrintSeparator letDefs
      , printString " in "
      , prettyPrint e
      ]
    where
      keyword
        | isRecursive flag = "letrec"
        | otherwise = "let"
  prettyPrint (EMatch# e matchCases)
    = printConcat
      [ printString "match "
      , prettyPrint e
      , printString " in"

      , printNewline

      , prettyPrintList prettyPrintSeparatorWithNewline matchCases
      ]

instance (PrettyPrintable a, PrettyPrintable (expr_ a)) => PrettyPrintable (MatchCase# expr_ a) where
  prettyPrint (tag, argBinders, bodyExpr)
    = printConcat
      [ printString "<"
      , printIntegral tag
      , printString ">"
      , prettyPrintList prettyPrintSpace argBinders
      , printString " -> "
      , prettyPrint bodyExpr
      ]

instance (PrettyPrintable a, PrettyPrintable (expr_ a)) => PrettyPrintable (LetDefinition# expr_ a) where
  prettyPrint (binder, bodyE)
    = printConcat
      [ prettyPrint binder
      , printString " = "
      , prettyPrint bodyE
      ]

instance PrettyPrintable Identifier where
  prettyPrint = printString

prettyPrintSeparatorWithNewline :: PrintSequence
prettyPrintSeparatorWithNewline = printString ";" `printAppend` printNewline

prettyPrintSeparator :: PrintSequence
prettyPrintSeparator = printString ";"

prettyPrintSpace :: PrintSequence
prettyPrintSpace = printString " "
