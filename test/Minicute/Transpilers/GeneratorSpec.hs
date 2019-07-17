{- HLINT ignore "Redundant do" -}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Minicute.Transpilers.GeneratorSpec
  ( spec
  ) where

import Test.Hspec

import Control.Monad
import Data.Tuple.Extra
import LLVM.AST.Global ( name, basicBlocks )
import LLVM.AST.Instruction ( Named( (:=) ) )
import Minicute.Transpilers.Generator
import Minicute.Utils.TH

import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as AST
import qualified LLVM.AST.Type as AST

spec :: Spec
spec = do
  describe "generateMachineCode" $ do
    forM_ testCases (uncurry3 generateMachineCodeTest)

generateMachineCodeTest :: TestName -> TestBeforeContent -> TestAfterContent -> SpecWith (Arg Expectation)
generateMachineCodeTest n beforeContent afterContent = do
  it ("generate a valid machine code from " <> n) $ do
    generateMachineCode beforeContent `shouldBe` afterContent

type TestName = String
type TestBeforeContent = GMachineProgram
type TestAfterContent = [AST.Definition]
type TestCase = (TestName, TestBeforeContent, TestAfterContent)

-- |
-- __TODO: Introduce an appropriate quasiquoter__
testCases :: [TestCase]
testCases
  = [ ( "an empty program"
      , [qqGMachine|
        |]
      , []
      )

    , ( "a program with a simple supercombinator"
      , [qqGMachine|
           f<0> {
             PushBasicValue 100;
             UpdateAsInteger 0;
             Return;
           }
        |]
      , [ AST.GlobalDefinition AST.functionDefaults
          { name = "__user_defined__f"
          , basicBlocks =
              [ AST.BasicBlock
                (AST.UnName 0)
                [ AST.UnName 1 := AST.Alloca AST.i32 Nothing 1 []
                , AST.Do
                  ( AST.Store
                    False
                    (AST.ConstantOperand (AST.Int 32 100))
                    (AST.LocalReference (AST.ptr AST.i32) (AST.UnName 1))
                    Nothing
                    1
                    []
                  )
                ]
                (AST.Do (AST.Ret Nothing []))
              ]
          }
        ]
      )
    ]
