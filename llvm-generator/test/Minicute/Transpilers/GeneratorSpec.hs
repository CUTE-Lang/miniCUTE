{- HLINT ignore "Redundant do" -}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Minicute.Transpilers.GeneratorSpec
  ( spec
  ) where

import Test.Hspec

import Control.Monad
import Data.Tuple.Extra
import LLVM.IRBuilder
import Minicute.Transpilers.Constants
import Minicute.Transpilers.Generator
import Minicute.Utils.TH

import qualified LLVM.AST as AST
import qualified LLVM.AST.Type as ASTT

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
      , execModuleBuilder emptyModuleBuilder
        ( do
            function "minicute__user__defined__f" [] ASTT.void . const
              $ do
              emitBlockStart "entry"

              -- PushBasicValue 100
              pName <- alloca ASTT.i32 Nothing 0
              store (operandInt 32 100) 0 pName
              vName <- load pName 0

              -- UpdateAsInteger 0
              sName <- load (AST.ConstantOperand constantAddrStackPointer) 0
              sName' <- gep sName [operandInt 32 0]
              nName <- load sName' 0
              _ <- call (AST.ConstantOperand constantUpdateNodeNInteger) [(vName, []), (nName, [])]

              -- Return
              bName <- load (AST.ConstantOperand constantAddrBasePointer) 0
              bName' <- gep bName [operandInt 32 0]
              store bName' 0 (AST.ConstantOperand constantAddrStackPointer)
              retVoid
        )
      )
    ]
