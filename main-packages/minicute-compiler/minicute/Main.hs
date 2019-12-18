-- |
-- Copyright: (c) 2018-present Junyoung Clare Jang
-- License: BSD 3-Clause
--
-- miniCUTE compiler
module Main
  ( main
  ) where

import Options

import Control.Lens.Operators
import Data.Text.Prettyprint.Doc.Minicute ( PrettyMC, prettyMC0 )
import Minicute.Parser.Minicute.Parser ( mainProgramMC )
import Minicute.Transpilers.Lifting.Lambda ( lambdaLifting )
import System.Environment
import System.IO
import Text.Megaparsec ( errorBundlePretty, parse )

main :: IO ()
main = do
  opts <- getArgs >>= parseArguments >>= elaborate
  compile $ inputHandle opts

compile :: Handle -> IO ()
compile handle = do
  putStrLn "inputs:"
  content <- hGetContents handle
  putStrLn ""
  case parse mainProgramMC "" content of
    Right program -> do
      printProgramLike ("program", program)
      let liftedProgram = lambdaLifting program
      printProgramLike ("lifted program", liftedProgram)
    Left err -> do
      putStrLn "error:"
      putStrLn (errorBundlePretty err)
  where
    printProgramLike :: (Show a, PrettyMC a) => (String, a) -> IO ()
    printProgramLike (name, program) = do
      putStrLn $ name <> " by show"
      print program
      putStrLn $ name <> " by pretty printer"
      print (prettyMC0 program)
