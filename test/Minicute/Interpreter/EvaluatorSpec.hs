module Minicute.Interpreter.EvaluatorSpec
  ( spec
  ) where

import Test.Hspec

import Minicute.Common.Program as P
import Minicute.Common.Value as V
import Minicute.Interpreter.Evaluator as E

spec :: Spec
spec = do
  describe "evaluateProgram" $ do
    it "evaluate valid program" $ do
      let program = P.Program (P.IntegerExpression 50)
      E.evaluateProgram program `shouldBe` V.IntegerValue 50
      let program = P.Program (P.IntegerExpression 24)
      E.evaluateProgram program `shouldBe` V.IntegerValue 24

  describe "evaluateExpression" $ do
    it "evaluate integer expression" $ do
      let expression = P.IntegerExpression 36
      E.evaluateExpression expression `shouldBe` V.IntegerValue 36
      let expression = P.IntegerExpression 2302
      E.evaluateExpression expression `shouldBe` V.IntegerValue 2302
