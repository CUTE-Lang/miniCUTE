module Language.Haskell.HLint.Minicute
  ( hlint
  ) where

import System.Directory
import System.FilePath

import qualified Language.Haskell.HLint3 as HLINT

-- |
-- __TODO: Autodetect 'dirs'__
hlint :: [FilePath] -> IO [HLINT.Idea]
hlint dirs = do
  maybeHlintPath <- findHlintPath
  case maybeHlintPath of
    Just hlintPath ->
      let
        hlintArgs = dirs <> ["--hint=" <> hlintPath]
      in
        HLINT.hlint hlintArgs
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
