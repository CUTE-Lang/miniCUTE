module Usage
  ( usage
  ) where

import System.Environment

usage :: IO ()
usage = do
  progName <- getProgName
  putStrLn $ "Usage: " <> progName <> " [file]"
