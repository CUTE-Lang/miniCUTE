{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Copyright: (c) 2018-present Junyoung Clare Jang
-- License: BSD 3-Clause
--
-- G-Machine interpreter of miniCUTE
module Main
  ( main
  ) where

import Control.Monad ( forM_ )
import Minicute.Interpreter.GMachine ( execGMachineT, interpretProgram )
import Minicute.Parser.GMachine.Parser ( gMachineProgram )
import System.Environment
import System.IO
import Text.Megaparsec

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text.Prettyprint.Doc as PP

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
      let indexedStates = NonEmpty.zip states [(0 :: Integer)..]
      forM_ indexedStates $ \(state, index) ->
        print
        $ PP.indent 2
          $ "state<" PP.<> PP.pretty index PP.<> ">:" PP.<> PP.hardline
          PP.<> PP.indent 2 (PP.pretty state)
    Left err -> do
      putStrLn "error:"
      putStrLn (errorBundlePretty err)

usage :: IO ()
usage = putStrLn "Usage: minicute-g [file]"
{-# INLINE usage #-}
