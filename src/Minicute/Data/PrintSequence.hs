module Minicute.Data.PrintSequence where

import Data.List

data PrintSequence
  = PrintNothing
  | PrintNewline
  | PrintString String
  | PrintAppend PrintSequence PrintSequence

toString :: PrintSequence -> String
toString ps = concat (flattenPrintSequence [ps])
  where
    flattenPrintSequence :: [PrintSequence] -> [String]
    flattenPrintSequence [] = []
    flattenPrintSequence (PrintNothing : pss) = flattenPrintSequence pss
    flattenPrintSequence (PrintNewline : pss) = "\n" : flattenPrintSequence pss
    flattenPrintSequence (PrintString s : pss) = s : flattenPrintSequence pss
    flattenPrintSequence (PrintAppend ps1 ps2 : pss) = flattenPrintSequence (ps1 : ps2 : pss)

printNothing :: PrintSequence
printNothing = PrintNothing

printNewline :: PrintSequence
printNewline = PrintNewline

printString :: String -> PrintSequence
printString = PrintString

printIntegral :: (Integral a, Show a) => a -> PrintSequence
printIntegral = PrintString . show

printAppend :: PrintSequence -> PrintSequence -> PrintSequence
printAppend = PrintAppend

printConcat :: [PrintSequence] -> PrintSequence
printConcat = foldl' PrintAppend PrintNothing

printIntersperse :: PrintSequence -> [PrintSequence] -> PrintSequence
printIntersperse = (printConcat .) . intersperse

printConditionalParentheses :: Bool -> PrintSequence -> PrintSequence
printConditionalParentheses withParenthesis ps
  | withParenthesis = printString "(" `printAppend` ps `printAppend` printString ")"
  | otherwise = ps
