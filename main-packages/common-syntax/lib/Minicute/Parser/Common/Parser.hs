module Minicute.Parser.Common.Parser
  ( primitive
  ) where

import Data.Functor
import Minicute.Data.Common
import Minicute.Parser.Common
import Text.Megaparsec

import qualified Minicute.Parser.Common.Lexer as L

primitive :: Parser Primitive
primitive
  = choice
    [ L.symbol "+" $> PrimAdd
    , L.symbol "-" $> PrimSub
    , L.symbol "*" $> PrimMul
    , L.symbol "/" $> PrimDiv
    ]

