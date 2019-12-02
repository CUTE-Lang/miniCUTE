{-# LANGUAGE NamedFieldPuns #-}
module Options
  ( ExternalOptions
  , _inputFilePath
  , parseArguments

  , InternalOptions
  , _inputHandle
  , internalize
  ) where

import Usage

import Control.Lens.Lens ( lens )
import Control.Lens.Operators
import Control.Lens.Type
import Control.Monad ( foldM )
import Data.Maybe
import System.Exit
import System.IO

newtype ExternalOptions
  = ExternalOptions
    { inputFilePath :: Maybe FilePath
    }

_inputFilePath :: Lens' ExternalOptions (Maybe FilePath)
_inputFilePath = lens inputFilePath setter
  where
    setter _ inputFilePath = ExternalOptions{ inputFilePath }

initialOptions :: ExternalOptions
initialOptions
  = ExternalOptions
    { inputFilePath = Nothing
    }

parseArguments :: [String] -> IO ExternalOptions
parseArguments = foldM parseArgument initialOptions

parseArgument :: ExternalOptions -> String -> IO ExternalOptions
parseArgument opts "-h" = usage >> exitSuccess
parseArgument opts "--help" = usage >> exitSuccess
parseArgument ExternalOptions{ inputFilePath = Just _ } _ = usage >> exitFailure
parseArgument opts filePath = pure opts{ inputFilePath = Just filePath }


newtype InternalOptions
  = InternalOptions
    { inputHandle :: Handle
    }

_inputHandle :: Lens' InternalOptions Handle
_inputHandle = lens inputHandle setter
  where
    setter _ inputHandle = InternalOptions{ inputHandle }

internalize :: ExternalOptions -> IO InternalOptions
internalize exOpts = do
  inputHandle <- getInputHandle exOpts
  pure
    $ InternalOptions
      { inputHandle
      }
  where
    getInputHandle = maybe (pure stdin) (`openFile` ReadMode) . inputFilePath
