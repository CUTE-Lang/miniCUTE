-- |
-- Copyright: (c) 2018-present Junyoung Clare Jang
-- License: BSD 3-Clause
--
-- HLint runner for miniCUTE
module Main
  ( main
  ) where

import Data.List
import System.Environment

main :: IO ()
main = do
  _ : _ : dst : args <- getArgs
  writeFile dst (makeMainFile args)

makeMainFile :: [String] -> String
makeMainFile args
  = intercalate "\n"
    [ "-- |"
    , "-- Copyright: (c) 2018-present Junyoung Clare Jang"
    , "-- License: BSD 3-Clause"
    , "--"
    , "-- An executable to run HLint"
    , "module Main"
    , "  ( main"
    , "  ) where"
    , ""
    , "import System.Environment"
    , "import Language.Haskell.HLint.Minicute ( hlint )"
    , ""
    , "main :: IO ()"
    , "main = do"
    , "  extraArgs <- getArgs"
    , "  hlint $ " <> show args <> " <> extraArgs"
    ]
