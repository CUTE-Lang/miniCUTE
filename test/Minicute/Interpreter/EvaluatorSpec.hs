module Minicute.Interpreter.EvaluatorSpec
  ( spec
  ) where

import Test.Hspec

import Minicute.Common.Program
import Minicute.Common.Value

import qualified Minicute.Interpreter.Evaluator as E

spec :: Spec
spec =
  do
    describe "evaluateProgram" $ do
      it "evaluate valid programs successfully" $ do
        let program = Program (IntegerExpression 50)
        E.evaluateProgram program `shouldBe` IntegerValue 50
        let program = Program (IntegerExpression 24)
        E.evaluateProgram program `shouldBe` IntegerValue 24

    describe "evaluateExpression" $ do
      it "evaluate integer expressions successfully" $ do
        let expression = IntegerExpression 36
        E.evaluateExpression expression `shouldBe` IntegerValue 36
        let expression = IntegerExpression 2302
        E.evaluateExpression expression `shouldBe` IntegerValue 2302

      it "evaluate operator expressions successfully" $ do
        let expression0 = IntegerExpression 10
            expression1 = IntegerExpression 15
            expression = OperatorExpression PlusOperator expression0 expression1
        E.evaluateExpression expression `shouldBe` IntegerValue 25
        let expression0 = IntegerExpression 10
            expression1 = IntegerExpression 15
            expression = OperatorExpression MinusOperator expression0 expression1
        E.evaluateExpression expression `shouldBe` IntegerValue (-5)
        let expression0 = IntegerExpression 2
            expression1 = IntegerExpression 5
            expression = OperatorExpression MultiplyOperator expression0 expression1
        E.evaluateExpression expression `shouldBe` IntegerValue 10
