module Main where

import Minicute.Data.PrintSequence ( toString )
import Minicute.Parser.Parser ( mainProgramL )
import Minicute.PrettyPrintable ( prettyPrint )
import Minicute.Transpiler.LambdaLifting ( lambdaLifting )
import System.Environment
import System.IO
import Text.Megaparsec ( errorBundlePretty, parse )

main :: IO ()
main = do
  args <- getArgs
  case args of
    _ : _ : _ -> usage
    filePath : _ -> openFile filePath ReadMode >>= compile
    [] -> compile stdin

compile :: Handle -> IO ()
compile handle = do
  putStrLn "inputs:"
  content <- hGetContents handle -- unlines <$> whileM (hIsEOF handle) (hGetLine handle)
  putChar '\n'
  case parse mainProgramL "" content of
    Right program -> do
      putStrLn "program by show:"
      print program
      putStrLn "program by pretty printing:"
      putStrLn (toString (prettyPrint program))
      let liftedProgram = lambdaLifting program
      putStrLn "lifted program by show:"
      print liftedProgram
      putStrLn "lifted program by pretty printing:"
      putStrLn (toString (prettyPrint liftedProgram))
    Left err -> do
      putStrLn "error:"
      putStrLn (errorBundlePretty err)

usage :: IO ()
usage = putStrLn "Usage: minicute [file]"
