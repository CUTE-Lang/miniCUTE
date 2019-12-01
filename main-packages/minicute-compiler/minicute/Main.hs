-- |
-- Copyright: (c) 2018-present Junyoung Clare Jang
-- License: BSD 3-Clause
--
-- miniCUTE compiler
module Main
  ( main
  ) where

import Control.Lens.Lens ( Lens, lens )
import Control.Lens.Operators
import Control.Monad ( foldM )
import Control.Monad.Identity ( Identity(..) )
import Data.Text.Prettyprint.Doc.Minicute ( prettyMC0 )
import Minicute.Parser.Minicute.Parser ( mainProgramMC )
import Minicute.Transpilers.Lifting.Lambda ( lambdaLifting )
import System.Environment
import System.Exit
import System.IO
import Text.Megaparsec ( errorBundlePretty, parse )

import qualified Data.List.NonEmpty as NonEmpty

main :: IO ()
main = do
  args <- getArgs
  options <- parseOptions args
  compile . runIdentity $ input options

compile :: Handle -> IO ()
compile handle = do
  putStrLn "inputs:"
  content <- hGetContents handle -- unlines <$> whileM (hIsEOF handle) (hGetLine handle)
  putChar '\n'
  case parse mainProgramMC "" content of
    Right program -> do
      putStrLn "program by show:"
      print program
      putStrLn "program by pretty printing:"
      print (prettyMC0 program)
      let liftedProgram = lambdaLifting program
      putStrLn "lifted program by show:"
      print liftedProgram
      putStrLn "lifted program by pretty printing:"
      print (prettyMC0 liftedProgram)
    Left err -> do
      putStrLn "error:"
      putStrLn (errorBundlePretty err)

-- |
-- _TODO: move this type and related functions to a library_
newtype Options ih
  = Options
    { input :: ih Handle
    }

_input :: Lens (Options ih) (Options ih') (ih Handle) (ih' Handle)
_input = lens input setter
  where
    setter opts input = Options{ input = input }

type PartialOptions = Options Maybe
type ValidOptions = Options Identity

parseOptions :: [String] -> IO ValidOptions
parseOptions = fmap elaborate . go initialOptions
  where
    elaborate = _input %~ elaboratedInputHandles
      where
        elaboratedInputHandles Nothing = Identity stdin
        elaboratedInputHandles (Just h) = Identity h

    go = foldM parseOption

parseOption :: PartialOptions -> String -> IO PartialOptions
parseOption opts "-h" = usage >> exitSuccess
parseOption opts "--help" = usage >> exitSuccess
parseOption Options{ input = Just _ } filePath = usage >> exitFailure
parseOption opts filePath
  = opts & _input %%~ (\_ -> Just <$> openFile filePath ReadMode)

initialOptions :: PartialOptions
initialOptions
  = Options
    { input = Nothing
    }

usage :: IO ()
usage = do
  progName <- getProgName
  putStrLn $ "Usage: " <> progName <> " [file]"
