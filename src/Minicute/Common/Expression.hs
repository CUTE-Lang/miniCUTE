module Minicute.Common.Expression where

data Operator
  = PlusOperator
  | MinusOperator
  | MultiplyOperator

data Expression
  = IntegerExpression Integer
  | OperatorExpression Operator Expression Expression
