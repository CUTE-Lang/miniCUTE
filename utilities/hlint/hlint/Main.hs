module Main
  ( main
  ) where

import Language.Haskell.HLint.Minicute ( hlint )
import System.Exit ( exitFailure, exitSuccess )

main :: IO ()
main =
  do
    -- This 'putStrLn' is to format stack test output
    putStrLn ""
    hints <- hlint ["lib"]
    if null hints
    then exitSuccess
    else exitFailure
