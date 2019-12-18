{-# LANGUAGE NamedFieldPuns #-}
module Options
  ( NaiveOptions( inputFilePath )
  , parseArguments

  , ElaboratedOptions( inputHandle )
  , elaborate
  ) where

import Usage

import Control.Monad ( foldM )
import Data.Maybe
import System.Exit
import System.IO

newtype NaiveOptions
  = NaiveOptions
    { inputFilePath :: Maybe FilePath
    }

initialOptions :: NaiveOptions
initialOptions
  = NaiveOptions
    { inputFilePath = Nothing
    }

parseArguments :: [String] -> IO NaiveOptions
parseArguments = foldM parseArgument initialOptions

parseArgument :: NaiveOptions -> String -> IO NaiveOptions
parseArgument _ "-h" = usage >> exitSuccess
parseArgument _ "--help" = usage >> exitSuccess
parseArgument opts@NaiveOptions{ inputFilePath = Nothing } filePath = pure opts{ inputFilePath = Just filePath }
parseArgument _ _ = usage >> exitFailure

newtype ElaboratedOptions
  = ElaboratedOptions
    { inputHandle :: Handle
    }

elaborate :: NaiveOptions -> IO ElaboratedOptions
elaborate opts = do
  inputHandle <- getInputHandle opts
  pure ElaboratedOptions{ inputHandle }
  where
    getInputHandle = maybe (pure stdin) (`openFile` ReadMode) . inputFilePath
