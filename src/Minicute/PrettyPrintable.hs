{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Minicute.PrettyPrintable
  ( PrettyPrintable( .. )
  ) where

import Minicute.Data.Fix
import Minicute.Data.PrintSequence
import Minicute.Types.Minicute.Precedence
import Minicute.Types.Minicute.Expression
import Minicute.Types.Minicute.Program

import qualified Data.Set as Set

class PrettyPrintable a where
  {-# MINIMAL (prettyPrint | prettyPrintPrec) #-}
  prettyPrint :: a -> PrintSequence
  prettyPrint = prettyPrintPrec 0
  prettyPrintPrec :: Int -> a -> PrintSequence
  prettyPrintPrec _ = prettyPrint

instance (PrettyPrintable a) => PrettyPrintable (Set.Set a) where
  prettyPrint set
    = printConcat
      [ printString "{{"
      , prettyPrintList (printString ", ") (Set.toList set)
      , printString "}}"
      ]

prettyPrintList :: (PrettyPrintable a) => PrintSequence -> [a] -> PrintSequence
prettyPrintList sep = printIntersperse sep . fmap prettyPrint
{-# INLINEABLE prettyPrintList #-}

instance (PrettyPrintable a, PrettyPrintable (expr a)) => PrettyPrintable (Program_ expr a) where
  prettyPrint (Program_ scs) = prettyPrintList prettyPrintSeparatorWithNewline scs

instance (PrettyPrintable a, PrettyPrintable (expr a)) => PrettyPrintable (Supercombinator_ expr a) where
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

instance (PrettyPrintable ann, PrettyPrintable a) => PrettyPrintable (AnnotatedExpressionL ann a) where
  prettyPrint (AELApplication2 ann2 ann1 (AELVariable annOp op) e1 e2)
    | op `elem` fmap fst binaryPrecedenceTable
    = printAnnotated [ann2, ann1, annOp] (prettyPrintBinaryExpressionPrec 0 op e1 e2)
  prettyPrint expr = prettyPrint (unFix2 expr)

instance (PrettyPrintable ann, PrettyPrintable a) => PrettyPrintable (AnnotatedExpression ann a) where
  prettyPrint (AEApplication2 ann2 ann1 (AEVariable annOp op) e1 e2)
    | op `elem` fmap fst binaryPrecedenceTable
    = printAnnotated [ann2, ann1, annOp] (prettyPrintBinaryExpressionPrec 0 op e1 e2)
  prettyPrint expr = prettyPrint (unFix2 expr)

instance (PrettyPrintable ann, PrettyPrintable a, PrettyPrintable (wExpr expr_ a)) => PrettyPrintable (AnnotatedExpression_ ann wExpr expr_ a) where
  prettyPrint (AnnotatedExpression_ (ann, expr)) = printAnnotated [ann] (prettyPrint expr)

printAnnotated :: (PrettyPrintable ann) => [ann] -> PrintSequence -> PrintSequence
printAnnotated anns exprSeq
  = printConcat
    [ printString "("
    , prettyPrintList prettyPrintSeparator anns
    , printString ", "
    , exprSeq
    , printString ")"
    ]
{-# INLINEABLE printAnnotated #-}

instance (PrettyPrintable a) => PrettyPrintable (ExpressionL a) where
  prettyPrintPrec prec (ELApplication2 (ELVariable op) e1 e2)
    | op `elem` fmap fst binaryPrecedenceTable
    = prettyPrintBinaryExpressionPrec prec op e1 e2
  prettyPrintPrec prec expr = prettyPrintPrec prec (unFix2 expr)

instance (PrettyPrintable a, PrettyPrintable (expr_ a)) => PrettyPrintable (ExpressionL_ expr_ a) where
  prettyPrintPrec prec (ELExpression_ expr_) = prettyPrintPrec prec expr_
  prettyPrintPrec prec (ELLambda_ argBinders bodyExpr)
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
  prettyPrintPrec prec expr = prettyPrintPrec prec (unFix2 expr)

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
{-# INLINEABLE prettyPrintBinaryExpressionPrec #-}

instance (PrettyPrintable a, PrettyPrintable (expr_ a)) => PrettyPrintable (Expression_ expr_ a) where
  prettyPrintPrec _ (EInteger_ int_) = printShowable int_
  prettyPrintPrec _ (EConstructor_ tag arity)
    = printConcat
      [ printString "$C{"
      , printShowable tag
      , printString ","
      , printShowable arity
      , printString "}"
      ]
  prettyPrintPrec _ (EVariable_ vId) = prettyPrint vId
  prettyPrintPrec prec (EApplication_ e1 e2)
    = printConditionalParentheses (prec > applicationPrecedence) . printIndented $ printConcat
      [ prettyPrintPrec applicationPrecedence e1
      , prettyPrintSpace
      , prettyPrintPrec applicationPrecedence1 e2
      ]
  prettyPrintPrec prec (ELet_ flag letDefs e)
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
  prettyPrintPrec prec (EMatch_ e matchCases)
    = printConditionalParentheses (prec > 0) . printIndented $ printConcat
      [ printString "match "
      , prettyPrint e
      , printString " with"

      , printNewline

      , prettyPrintDoubleSpace
      , printIndented (prettyPrintList prettyPrintSeparatorWithNewline matchCases)
      ]

instance (PrettyPrintable a, PrettyPrintable (expr_ a)) => PrettyPrintable (MatchCase_ expr_ a) where
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

instance (PrettyPrintable a, PrettyPrintable (expr_ a)) => PrettyPrintable (LetDefinition_ expr_ a) where
  prettyPrint (binder, bodyExpr)
    = printConcat
      [ prettyPrint binder
      , printString " = "
      , prettyPrint bodyExpr
      ]

instance PrettyPrintable Identifier where
  prettyPrint = printString

binaryPrecedenceTable :: PrecedenceTable
binaryPrecedenceTable = filter (isInfix . snd) defaultPrecedenceTable
{-# INLINEABLE binaryPrecedenceTable #-}

prettyPrintSeparatorWithNewline :: PrintSequence
prettyPrintSeparatorWithNewline = printString ";" `printAppend` printNewline
{-# INLINEABLE prettyPrintSeparatorWithNewline #-}

prettyPrintSeparator :: PrintSequence
prettyPrintSeparator = printString ";"
{-# INLINEABLE prettyPrintSeparator #-}

prettyPrintSpace :: PrintSequence
prettyPrintSpace = printString " "
{-# INLINEABLE prettyPrintSpace #-}

prettyPrintDoubleSpace :: PrintSequence
prettyPrintDoubleSpace = printString "  "
{-# INLINEABLE prettyPrintDoubleSpace #-}
