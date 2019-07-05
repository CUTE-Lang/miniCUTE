{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Common types for parsers
module Minicute.Parser.Common
  ( MonadParser

  , Parser
  ) where

import Data.Void
import Text.Megaparsec

-- |
-- @MonadParser@ is a constraint used to write generic parsers.
type MonadParser e s m = (MonadParsec e s m, ShowErrorComponent e, s ~ String)

-- |
-- @Parser@ is a concrete type for parsers.
type Parser = Parsec Void String
