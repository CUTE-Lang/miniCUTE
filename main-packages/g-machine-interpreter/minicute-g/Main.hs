module Main
  ( main
  ) where

import Minicute.Parser.GMachine.Parser ( gMachineProgram )
import System.Environment
import System.IO
import Text.Megaparsec

main :: IO ()
main = do
  args <- getArgs
  case args of
    _ : _ : _ -> usage
    filePath : _ -> openFile filePath ReadMode >>= interpret
    [] -> interpret stdin

interpret :: Handle -> IO ()
interpret handle = do
  content <- hGetContents handle
  case parse gMachineProgram "" content of
    Right program -> do
      putStrLn "program by show:"
      print program
    Left err -> do
      putStrLn "error:"
      putStrLn (errorBundlePretty err)

usage :: IO ()
usage = putStrLn "Usage: minicute-g [file]"
