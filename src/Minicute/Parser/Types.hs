{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Minicute.Parser.Types
  ( MonadParser

  , WithPrecedence

  , Parser

  , Precedence( .. )
  , PrecedenceTableEntry
  , PrecedenceTable
  , defaultPrecedenceTable
  ) where

import Control.Monad.Reader ( ReaderT )
import Data.Void
import Minicute.Parser.Precedence
import Text.Megaparsec

type MonadParser e s m = (MonadParsec e s m, ShowErrorComponent e, s ~ String)

type WithPrecedence m = ReaderT PrecedenceTable m

type Parser = Parsec Void String
