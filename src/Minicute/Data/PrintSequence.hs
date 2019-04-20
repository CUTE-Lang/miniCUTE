module Minicute.Data.PrintSequence where

data PrintSequence

toString :: PrintSequence -> String
toString = undefined

printNothing :: PrintSequence
printNothing = undefined

printNewline :: PrintSequence
printNewline = undefined

printString :: String -> PrintSequence
printString = undefined

printIntegral :: (Integral a) => a -> PrintSequence
printIntegral = undefined

printAppend :: PrintSequence -> PrintSequence -> PrintSequence
printAppend = undefined

printConcat :: [PrintSequence] -> PrintSequence
printConcat = undefined

printInterleave :: PrintSequence -> [PrintSequence] -> PrintSequence
printInterleave = undefined
