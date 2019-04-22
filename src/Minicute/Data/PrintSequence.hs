module Minicute.Data.PrintSequence
  ( PrintSequence

  , toString

  , printNothing
  , printNewline
  , printString
  , printIndented
  , printAppend

  , printIntegral
  , printConcat
  , printIntersperse
  , printConditionalParentheses
  ) where

import Data.List

data PrintSequence
  = PrintNothing
  | PrintNewline
  | PrintString String
  | PrintIndented PrintSequence
  | PrintAppend PrintSequence PrintSequence

toString :: PrintSequence -> String
toString ps = concat (flatten initialFgs [(ps, initialFls)])

printNothing :: PrintSequence
printNothing = PrintNothing

printNewline :: PrintSequence
printNewline = PrintNewline

printString :: String -> PrintSequence
printString = printIntersperse printNewline . fmap PrintString . lines . toUnix

printIndented :: PrintSequence -> PrintSequence
printIndented = PrintIndented

printAppend :: PrintSequence -> PrintSequence -> PrintSequence
printAppend = PrintAppend

printIntegral :: (Integral a, Show a) => a -> PrintSequence
printIntegral = printString . show

printConcat :: [PrintSequence] -> PrintSequence
printConcat = foldl' printAppend PrintNothing

printIntersperse :: PrintSequence -> [PrintSequence] -> PrintSequence
printIntersperse = (printConcat .) . intersperse

printConditionalParentheses :: Bool -> PrintSequence -> PrintSequence
printConditionalParentheses withParenthesis ps
  | withParenthesis = printString "(" `printAppend` ps `printAppend` printString ")"
  | otherwise = ps

flatten :: FlattenGlobalState -> [(PrintSequence, FlattenLocalState)] -> [String]
flatten _ [] = []
flatten fgs ((PrintNothing, _) : pss) = flatten fgs pss
flatten _ ((PrintNewline, fls) : pss) = flsCreateNewline fls : flatten (flsToFgs fls) pss
flatten fgs ((PrintString s, _) : pss) = s : flatten (fgsUpdateColumn fgs s) pss
flatten fgs ((PrintIndented s, _) : pss) = flatten fgs ((s, fgsToFls fgs) : pss)
flatten fgs ((PrintAppend ps1 ps2, fls) : pss) = flatten fgs ((ps1, fls) : (ps2, fls) : pss)

type FlattenGlobalState = Int -- ^ Current column

initialFgs :: FlattenGlobalState
initialFgs = 0

fgsUpdateColumn :: FlattenGlobalState -> String -> FlattenGlobalState
fgsUpdateColumn col s = col + length s

type FlattenLocalState = Int -- ^ Indentation for specific sequence

initialFls :: FlattenLocalState
initialFls = 0

flsCreateNewline :: FlattenLocalState -> String
flsCreateNewline fls = "\n" <> replicate fls ' '

fgsToFls :: FlattenGlobalState -> FlattenLocalState
fgsToFls fgs = fgs

flsToFgs :: FlattenLocalState -> FlattenGlobalState
flsToFgs fls = fls

toUnix :: String -> String
toUnix ('\r' : '\n' : cs) = '\n' : toUnix cs
toUnix ('\r' : cs) = '\n' : toUnix cs
toUnix (c : cs) = c : toUnix cs
toUnix [] = []
