module Main
  ( main
  ) where

import Control.Monad ( forM_ )
import Data.List.NonEmpty ( toList )
import Minicute.Interpreter.GMachine ( execGMachineT, interpretProgram )
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
      states <- execGMachineT (interpretProgram program)
      putStrLn "Execution states:"
      let indexedStates = zip (toList states) [(0 :: Integer)..]
      forM_ indexedStates $ \(state, index) -> do
        putStrLn $ "  state<" <> show index <> ">:"
        putStrLn $ "    " <> show  state
    Left err -> do
      putStrLn "error:"
      putStrLn (errorBundlePretty err)

usage :: IO ()
usage = putStrLn "Usage: minicute-g [file]"
