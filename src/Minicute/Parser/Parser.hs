module Minicute.Parser.Parser
  ( L.Parser
  , program
  ) where

import Control.Monad.Combinators.Expr
import Data.Void
import Text.Megaparsec

import qualified Minicute.Parser.Lexer as L
import qualified Minicute.Common.Program as Prog

program :: L.Parser Prog.Program
program = (Prog.Program <$> expression) <* eof

expression :: L.Parser Prog.Expression
expression = makeExprParser integerExpression operatorTable

integerExpression :: L.Parser Prog.Expression
integerExpression = Prog.IntegerExpression <$> L.integer

operatorTable :: [[Operator L.Parser Prog.Expression]]
operatorTable =
  [ [ InfixL (Prog.OperatorExpression Prog.MultiplyOperator <$ L.symbol "*")
    ]
  , [ InfixL (Prog.OperatorExpression Prog.PlusOperator <$ L.symbol "+")
    , InfixL (Prog.OperatorExpression Prog.MinusOperator <$ L.symbol "-")
    ]
  ]
