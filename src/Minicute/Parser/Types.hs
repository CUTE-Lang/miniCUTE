{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Minicute.Parser.Types
  ( MonadParser

  , Parser
  ) where

import Data.Void
import Text.Megaparsec

type MonadParser e s m = (MonadParsec e s m, ShowErrorComponent e, s ~ String)

type Parser = Parsec Void String
