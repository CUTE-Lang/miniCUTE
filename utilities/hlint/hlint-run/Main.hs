module Main
  ( main
  ) where

import System.Environment

main :: IO ()
main = do
  _ : _ : dst : args <- getArgs
  writeFile dst (makeMainFile args)

makeMainFile :: [String] -> String
makeMainFile args =
  ( showString "module Main\n"
  . showString "  ( main\n"
  . showString "  ) where\n"
  . showString "\n"
  . showString "import Language.Haskell.HLint.Minicute ( hlint )\n"
  . showString "\n"
  . showString "main :: IO ()\n"
  . showString "main = hlint " . shows args
  ) "\n"
