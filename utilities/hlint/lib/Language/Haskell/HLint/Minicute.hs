-- |
-- Copyright: (c) 2018-present Junyoung Clare Jang
-- License: BSD 3-Clause
--
-- HLint utility functions for miniCUTE
module Language.Haskell.HLint.Minicute
  ( hlint
  ) where

import Prelude hiding ( fail )

import Control.Monad ( unless )
import Control.Monad.Fail
import Data.Maybe ( fromMaybe )
import System.Directory ( canonicalizePath, findFile, getCurrentDirectory )
import System.Exit ( exitFailure )
import System.FilePath ( isDrive, (</>) )

import qualified Language.Haskell.HLint4 as HLINT

hlint :: [FilePath] -> IO ()
hlint dirs = do
  -- This 'putStrLn' is to format stack test output
  putStrLn ""
  hlintPath <- fromMaybe hlintFailure <$> findHlintPath
  hints <- HLINT.hlint (dirs <> ["--hint=" <> hlintPath])
  unless (null hints)
    exitFailure
  where
    hlintFailure
      = fail "The setting file hlint.yaml does not exist in any ancestor directories"
    {-# INLINABLE hlintFailure #-}

findHlintPath :: IO (Maybe FilePath)
findHlintPath = do
  dirs <- ancestorDirectories
  findFile dirs ".hlint.yaml"
  where
    ancestorDirectories = getCurrentDirectory >>= ancestorDirectoriesFrom

    ancestorDirectoriesFrom cDir
      = (cDir :)
        <$>
        ( getParentDirectory cDir
          >>= maybe (pure [] :: IO [FilePath]) ancestorDirectoriesFrom
        )

    getParentDirectory dir
      | isDrive dir = pure Nothing
      | otherwise = Just <$> canonicalizePath (dir </> "..")

    {-# INLINABLE ancestorDirectories #-}
    {-# INLINABLE ancestorDirectoriesFrom #-}
    {-# INLINABLE getParentDirectory #-}
{-# INLINABLE findHlintPath #-}
