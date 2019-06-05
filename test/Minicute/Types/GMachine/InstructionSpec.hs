{- HLINT ignore "Redundant do" -}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Minicute.Types.GMachine.InstructionSpec
  ( spec
  ) where

import Test.Hspec

import Control.Monad
import Data.Tuple.Extra
import Minicute.Types.GMachine.Instruction
import Minicute.Types.Minicute.Program
import Minicute.Utils.TH

spec :: Spec
spec = do
  describe "transpileProgram" $ do
    forM_ testCases (uncurry3 transpileProgramTest)

transpileProgramTest :: TestName -> TestBeforeContent -> TestAfterContent -> SpecWith (Arg Expectation)
transpileProgramTest name beforeContent afterContent = do
  it ("transpile " <> name <> " into valid GMachine program") $ do
    transpileProgram beforeContent `shouldBe` afterContent

type TestName = String
type TestBeforeContent = MainProgram
type TestAfterContent = GMachineProgram
type TestCase = (TestName, TestBeforeContent, TestAfterContent)

testCases :: [TestCase]
testCases
  = [ ( "empty program"
      , [qqMiniMain|
        |]
      , [
        ]
      )

    , ( "program with constant top-level definition"
      , [qqMiniMain|
                   f = 5;
                   g = $C{9;0}
        |]
      , [ ( "f"
          , 0
          , [ IPushBasicValue 5
            , IUpdateAsInteger 0
            ]
          )
        , ( "g"
          , 0
          , [ IPushBasicValue 9
            , IUpdateAsConstructor 0
            ]
          )
        ]
      )

    , ( "program with an argument"
      , [qqMiniMain|
                   f x = x;
        |]
      , [ ( "f"
          , 1
          , [ ICopyArgument 1
            , IUpdate 2
            , IPop 2
            , IUnwind
            ]
          )
        ]
      )

    , ( "program with a simple application"
      , [qqMiniMain|
                   f = g 4;
                   g x = x;
        |]
      , [ ( "f"
          , 0
          , [ IMakeGlobal "g"
            , IMakeInteger 4
            , IMakeApplication
            , IUpdate 1
            , IPop 1
            , IUnwind
            ]
          )
        , ( "g"
          , 1
          , [ ICopyArgument 1
            , IUpdate 2
            , IPop 2
            , IUnwind
            ]
          )
        ]
      )

    , ( "program with a constructor application"
      , [qqMiniMain|
                   f = $C{1;1} 4;
                   g x = $C{3;2} x
        |]
      , [ ( "f"
          , 0
          , [ IMakeConstructor 1 1
            , IMakeInteger 4
            , IMakeApplication
            , IUpdate 1
            , IPop 1
            , IUnwind
            ]
          )
        , ( "g"
          , 1
          , [ IMakeConstructor 3 2
            , ICopyArgument 2
            , IMakeApplication
            , IUpdate 2
            , IPop 2
            , IUnwind
            ]
          )
        ]
      )
    ]
