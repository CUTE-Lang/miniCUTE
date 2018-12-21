module Minicute.Common.Expression where

data Operator
  = PlusOperator
  | MinusOperator
  | MultiplyOperator
  deriving ( Eq, Show )

data Expression
  = IntegerExpression Integer
  | OperatorExpression Operator Expression Expression
  deriving ( Eq, Show )
