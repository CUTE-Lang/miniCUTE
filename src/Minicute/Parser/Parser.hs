module Minicute.Parser.Parser
  ( L.Parser
  , P.MainProgramL

  , programL
  ) where

import Control.Monad.Combinators.Expr
import Data.Void
import Text.Megaparsec

import qualified Minicute.Parser.Lexer as L
import qualified Minicute.Types.Program as P

programL :: L.Parser P.MainProgramL
programL = undefined
