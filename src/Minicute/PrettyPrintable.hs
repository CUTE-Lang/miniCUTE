{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Minicute.PrettyPrintable
  ( PrettyPrintable( .. )
  ) where

import Minicute.Data.Fix
import Minicute.Data.PrintSequence
import Minicute.Types.Minicute.Precedence
import Minicute.Types.Minicute.Expression
import Minicute.Types.Minicute.Program

class PrettyPrintable a where
  {-# MINIMAL (prettyPrint | prettyPrintPrec) #-}
  prettyPrint :: a -> PrintSequence
  prettyPrint = prettyPrintPrec 0
  prettyPrintPrec :: Int -> a -> PrintSequence
  prettyPrintPrec _ = prettyPrint

prettyPrintList :: (PrettyPrintable a) => PrintSequence -> [a] -> PrintSequence
prettyPrintList sep = printIntersperse sep . fmap prettyPrint

instance (PrettyPrintable a, PrettyPrintable expr) => PrettyPrintable (Program# a expr) where
  prettyPrint (Program# scs) = prettyPrintList prettyPrintSeparatorWithNewline scs

instance (PrettyPrintable a, PrettyPrintable expr) => PrettyPrintable (Supercombinator# a expr) where
  prettyPrint (scId, argBinders, expr)
    = printConcat
      [ prettyPrint scId
      , if null argBinders
        then printNothing
        else prettyPrintSpace
      , prettyPrintList prettyPrintSpace argBinders
      , printString " = "
      , prettyPrint expr
      ]

instance (PrettyPrintable a) => PrettyPrintable (ExpressionL a) where
  prettyPrintPrec prec (ELApplication2 (ELVariable op) e1 e2)
    | op `elem` fmap fst binaryPrecedenceTable
    = prettyPrintBinaryExpressionPrec prec op e1 e2
  prettyPrintPrec prec expr
    = case expr of
        Fix2 expr# -> prettyPrintPrec prec expr#

instance (PrettyPrintable a, PrettyPrintable (expr_ a)) => PrettyPrintable (ExpressionL# expr_ a) where
  prettyPrintPrec prec (ELExpression# expr#) = prettyPrintPrec prec expr#
  prettyPrintPrec prec (ELLambda# argBinders bodyExpr)
    = printConditionalParentheses (prec > 0) . printIndented $ printConcat
      [ printString "\\"
      , prettyPrintList prettyPrintSpace argBinders
      , printString " ->"

      , printNewline

      , prettyPrintDoubleSpace
      , prettyPrint bodyExpr
      ]

instance (PrettyPrintable a) => PrettyPrintable (Expression a) where
  prettyPrintPrec prec (EApplication2 (EVariable op) e1 e2)
    | op `elem` fmap fst binaryPrecedenceTable
    = prettyPrintBinaryExpressionPrec prec op e1 e2
  prettyPrintPrec prec expr
    = case expr of
        Fix2 expr# -> prettyPrintPrec prec expr#

prettyPrintBinaryExpressionPrec :: (PrettyPrintable a, PrettyPrintable (expr_ a)) => Int -> Identifier -> expr_ a -> expr_ a -> PrintSequence
prettyPrintBinaryExpressionPrec prec op e1 e2
  = printConditionalParentheses (prec > opPrec) . printIndented $ printConcat
    [ prettyPrintPrec leftPrec e1
    , prettyPrintSpace
    , prettyPrint op
    , prettyPrintSpace
    , prettyPrintPrec rightPrec e2
    ]
  where
    (leftPrec, opPrec, rightPrec)
      = case lookup op binaryPrecedenceTable of
          Just (PInfixN p) -> (p + 1, p, p + 1)
          Just (PInfixL p) -> (p, p, p + 1)
          Just (PInfixR p) -> (p + 1, p, p)
          _ -> (applicationPrecedence1, applicationPrecedence, applicationPrecedence1)

instance (PrettyPrintable a, PrettyPrintable (expr_ a)) => PrettyPrintable (Expression# expr_ a) where
  prettyPrintPrec _ (EInteger# int#) = printShowable int#
  prettyPrintPrec _ (EConstructor# tag arity)
    = printConcat
      [ printString "$C{"
      , printShowable tag
      , printString ","
      , printShowable arity
      , printString "}"
      ]
  prettyPrintPrec _ (EVariable# vId) = prettyPrint vId
  prettyPrintPrec prec (EApplication# e1 e2)
    = printConditionalParentheses (prec > applicationPrecedence) . printIndented $ printConcat
      [ prettyPrintPrec applicationPrecedence e1
      , prettyPrintSpace
      , prettyPrintPrec applicationPrecedence1 e2
      ]
  prettyPrintPrec prec (ELet# flag letDefs e)
    = printConditionalParentheses (prec > 0) . printIndented $ printConcat
      [ printString keyword

      , printNewline

      , prettyPrintDoubleSpace
      , printIndented (prettyPrintList prettyPrintSeparator letDefs)

      , printNewline

      , printString "in"

      , printNewline

      , prettyPrintDoubleSpace
      , prettyPrint e
      ]
    where
      keyword
        | isRecursive flag = "letrec"
        | otherwise = "let"
  prettyPrintPrec prec (EMatch# e matchCases)
    = printConditionalParentheses (prec > 0) . printIndented $ printConcat
      [ printString "match "
      , prettyPrint e
      , printString " with"

      , printNewline

      , prettyPrintDoubleSpace
      , printIndented (prettyPrintList prettyPrintSeparatorWithNewline matchCases)
      ]

instance (PrettyPrintable a, PrettyPrintable (expr_ a)) => PrettyPrintable (MatchCase# expr_ a) where
  prettyPrint (tag, argBinders, bodyExpr)
    = printConcat
      [ printString "<"
      , printShowable tag
      , printString ">"
      , if null argBinders
        then printNothing
        else prettyPrintSpace
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

binaryPrecedenceTable :: PrecedenceTable
binaryPrecedenceTable = filter (isInfix . snd) defaultPrecedenceTable

prettyPrintSeparatorWithNewline :: PrintSequence
prettyPrintSeparatorWithNewline = printString ";" `printAppend` printNewline

prettyPrintSeparator :: PrintSequence
prettyPrintSeparator = printString ";"

prettyPrintSpace :: PrintSequence
prettyPrintSpace = printString " "

prettyPrintDoubleSpace :: PrintSequence
prettyPrintDoubleSpace = printString "  "
