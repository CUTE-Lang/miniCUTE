module Minicute.Interpreter.Evaluator where

import qualified Minicute.Common.Program as P
import qualified Minicute.Common.Value as V

evaluateProgram :: P.Program -> V.Value
evaluateProgram (P.Program e) = evaluateExpression e

evaluateExpression :: P.Expression -> V.Value
evaluateExpression (P.IntegerExpression i) = V.IntegerValue i
evaluateExpression (P.OperatorExpression op e0 e1) = evaluateOperatorExpression op e0 e1

evaluateOperatorExpression :: P.Operator -> P.Expression -> P.Expression -> V.Value
evaluateOperatorExpression op e0 e1 =
  case (evaluateExpression e0, evaluateExpression e1) of
    (V.IntegerValue i0, V.IntegerValue i1) -> V.IntegerValue $ opFun i0 i1
    _ -> V.ErrorValue
  where
    opFun =
      case op of
        P.PlusOperator -> (+)
        P.MinusOperator -> (-)
        P.MultiplyOperator -> (*)
