module Language.Haskell.HLint.Minicute
  ( hlint
  ) where

import Prelude hiding ( fail )

import Control.Monad.Fail
import System.Directory ( canonicalizePath, getCurrentDirectory, findFile )
import System.Exit ( exitFailure, exitSuccess )
import System.FilePath ( isDrive, (</>) )

import qualified Language.Haskell.HLint3 as HLINT

hlint :: [FilePath] -> IO ()
hlint dirs = do
  -- This 'putStrLn' is to format stack test output
  putStrLn ""
  maybeHlintPath <- findHlintPath
  case maybeHlintPath of
    Just hlintPath ->
      let
        hlintArgs = dirs <> ["--hint=" <> hlintPath]
      in do
        hints <- HLINT.hlint hlintArgs
        if null hints
        then exitSuccess
        else exitFailure
    Nothing ->
      fail "The setting file hlint.yaml does not exist in any ancestor directories"

findHlintPath :: IO (Maybe FilePath)
findHlintPath = do
  dirs <- ancestorDirectories
  findFile dirs "hlint.yaml"
  where
    ancestorDirectories :: IO [FilePath]
    ancestorDirectories = getCurrentDirectory >>= ancestorDirectoriesFrom

    ancestorDirectoriesFrom :: FilePath -> IO [FilePath]
    ancestorDirectoriesFrom cDir
      = (cDir :)
        <$>
        ( getParentDirectory cDir
          >>= maybe (pure []) ancestorDirectoriesFrom
        )

    getParentDirectory :: FilePath -> IO (Maybe FilePath)
    getParentDirectory dir
      = if isDrive dir
        then pure Nothing
        else Just <$> canonicalizePath (dir </> "..")
