module Usage
  ( usage
  ) where

import Data.Foldable
import System.Environment

-- |
-- __TODO: generate this using options__
usage :: IO ()
usage = do
  progName <- getProgName
  traverse_ putStrLn
    [ "Usage: " <> progName <> " [OPTIONS]... [FILE]"
    , "Execute miniCUTE compile steps against FILE."
    , "With no FILE, read standard input."
    , ""
    , "Options:"
    , "  --help, -h        display this message and exit successfully"
    ]
