-- |
-- Copyright: (c) 2018-present Junyoung Clare Jang
-- License: BSD 3-Clause
--
-- HLint runner for miniCUTE
module Main
  ( main
  ) where

import System.Environment

main :: IO ()
main = do
  _ : _ : dst : args <- getArgs
  writeFile dst (makeMainFile args)

makeMainFile :: [String] -> String
makeMainFile args
  = ( showString ""
      . showString "-- |\n"
      . showString "-- Copyright: (c) 2018-present Junyoung Clare Jang\n"
      . showString "-- License: BSD 3-Clause\n"
      . showString "--\n"
      . showString "-- An executable to run HLint\n"
      . showString "module Main\n"
      . showString "  ( main\n"
      . showString "  ) where\n"
      . showString "\n"
      . showString "import Language.Haskell.HLint.Minicute ( hlint )\n"
      . showString "\n"
      . showString "main :: IO ()\n"
      . showString "main = hlint " . shows args
    ) "\n"
