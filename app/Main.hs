module Main where

import Control.Monad ( liftM2 )
import Minicute.Data.PrintSequence ( toString )
import Minicute.Parser.Parser ( programL )
import Minicute.PrettyPrintable ( prettyPrint )
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
  content <- unlines <$> whileM (hIsEOF handle) (hGetLine handle)
  putChar '\n'
  case parse programL "" content of
    Right expr -> do
      putStrLn "outputs by show:"
      print expr
      putStrLn "outputs by pretty printing:"
      putStrLn (toString (prettyPrint expr))
    Left err -> do
      putStrLn "error:"
      putStrLn (errorBundlePretty err)

whileM :: (Monad m) => m Bool -> m a -> m [a]
whileM cond act = go
  where
    go = do
      b <- cond
      if b
      then return []
      else liftM2 (:) act go

usage :: IO ()
usage = putStrLn "Usage: minicute [file]"
