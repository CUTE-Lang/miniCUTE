{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Copyright: (c) 2018-present Junyoung Clare Jang
-- License: BSD 3-Clause
--
-- Common types for parsers
module Minicute.Parser.Common
  ( MonadParser

  , Parser
  ) where

import Data.Void
import Text.Megaparsec

-- |
-- @MonadParser@ is a constraint used to write generic parsers.
type MonadParser e s m = (MonadParsec e s m, ShowErrorComponent e, s ~ String, MonadFail m)

-- |
-- @Parser@ is a concrete type for parsers.
type Parser = Parsec Void String
