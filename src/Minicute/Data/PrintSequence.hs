module Minicute.Data.PrintSequence
  ( PrintSequence

  , toString

  , printNothing
  , printNewline
  , printString
  , printIndented
  , printAppend

  , printShowable
  , printConcat
  , printIntersperse
  , printConditionalParentheses
  ) where

import Data.List
import Minicute.Data.String

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
printAppend PrintNothing = id
printAppend s1 = PrintAppend s1
infixr 9 `printAppend`

printShowable :: (Show a) => a -> PrintSequence
printShowable = printString . show

printConcat :: (Foldable t) => t PrintSequence -> PrintSequence
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
flatten fgs ((PrintString str, _) : pss) = str : flatten (fgsUpdateColumn fgs str) pss
flatten fgs ((PrintIndented ps, _) : pss) = flatten fgs ((ps, fgsToFls fgs) : pss)
flatten fgs ((PrintAppend ps1 ps2, fls) : pss) = flatten fgs ((ps1, fls) : (ps2, fls) : pss)

type FlattenGlobalState = Int -- ^ Current column

initialFgs :: FlattenGlobalState
initialFgs = 0

fgsUpdateColumn :: FlattenGlobalState -> String -> FlattenGlobalState
fgsUpdateColumn fgs s = fgs + length s

type FlattenLocalState = Int -- ^ Indentation for specific sequence

initialFls :: FlattenLocalState
initialFls = 0

flsCreateNewline :: FlattenLocalState -> String
flsCreateNewline fls = "\n" <> replicate fls ' '

fgsToFls :: FlattenGlobalState -> FlattenLocalState
fgsToFls fgs = fgs

flsToFgs :: FlattenLocalState -> FlattenGlobalState
flsToFgs fls = fls
