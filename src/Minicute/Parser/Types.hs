module Minicute.Parser.Types
  ( Parser
  , ParserWithPrecedence

  , Precedence( .. )
  , PrecedenceTableEntry
  , PrecedenceTable
  , defaultPrecedenceTable
  ) where

import Control.Monad.Reader
import Data.Void
import Minicute.Parser.Precedence
import Text.Megaparsec

type Parser = Parsec Void String

-- |
-- 'ParserWithPrecedence' is parser after precedence collecting
type ParserWithPrecedence = ReaderT PrecedenceTable Parser
