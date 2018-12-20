module Minicute.Interpreter.Evaluator where

import qualified Minicute.Common.Program as P
import qualified Minicute.Common.Value as V

evaluateProgram :: P.Program -> V.Value
evaluateProgram (P.Program e) = evaluateExpression e

evaluateExpression :: P.Expression -> V.Value
evaluateExpression (P.IntegerExpression i) = V.IntegerValue i
