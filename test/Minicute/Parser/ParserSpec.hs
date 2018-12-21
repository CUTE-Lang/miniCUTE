module Minicute.Parser.ParserSpec
  ( spec
  ) where

import Minicute.Parser.TestUtils
import Test.Hspec

import Data.Either
import Minicute.Common.Program

import qualified Minicute.Parser.Parser as P

cases :: [(String, Program)]
cases =
  [ ( "20"
    , Program
      ( IntegerExpression 20
      )
    )
  , ( "20 + 3"
    , Program
      ( OperatorExpression
        PlusOperator
        ( IntegerExpression 20
        )
        ( IntegerExpression 3
        )
      )
    )
  , ( "2 * 3 + 1"
    , Program
      ( OperatorExpression
        PlusOperator
        ( OperatorExpression
          MultiplyOperator
          ( IntegerExpression 2
          )
          ( IntegerExpression 3
          )
        )
        ( IntegerExpression 1
        )
      )
    )
  ]

spec :: Spec
spec = do
  describe "program parser" $ do
    it "parses valid programs successfullly" $ do
      runParserTest P.program (fst $ cases!!0) `shouldBe` Right (snd $ cases!!0)
      runParserTest P.program (fst $ cases!!1) `shouldBe` Right (snd $ cases!!1)
      runParserTest P.program (fst $ cases!!2) `shouldBe` Right (snd $ cases!!2)
