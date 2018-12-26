module Minicute.Parser.ParserSpec
  ( spec
  ) where

import Minicute.Parser.TestUtils
import Test.Hspec

import Control.Monad
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
  , ( "2 * (3 + 1)"
    , Program
      ( OperatorExpression
        MultiplyOperator
        ( IntegerExpression 2
        )
        ( OperatorExpression
          PlusOperator
          ( IntegerExpression 3
          )
          ( IntegerExpression 1
          )
        )
      )
    )
  ]

spec :: Spec
spec =
  do
    describe "program parser" $ do
      it "parses valid programs successfullly" $ do
        let
          runCase (s, v) = runParserTest P.program s `shouldBe` Right v
        forM_ cases runCase
