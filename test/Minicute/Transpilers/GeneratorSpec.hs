{- HLINT ignore "Redundant do" -}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Minicute.Transpilers.GeneratorSpec
  ( spec
  ) where

import Test.Hspec

import Control.Monad
import Data.Tuple.Extra
import Data.Word
import LLVM.AST.Global ( name, returnType, basicBlocks )
import LLVM.IRBuilder
import Minicute.Transpilers.Generator
import Minicute.Utils.TH

import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as ASTC
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
              pName <- emitInstr typeInt32Ptr (AST.Alloca ASTT.i32 Nothing 0 [])
              emitInstrVoid (AST.Store False (operandInt 32 100) pName Nothing 0 [])
              vName <- emitInstr typeInt32 (AST.Load False (AST.LocalReference typeInt32Ptr (AST.UnName 0)) Nothing 0 [])

              -- UpdateAsInteger 0
              nName <- emitInstr typeNodeNIntegerPtr (AST.BitCast (AST.ConstantOperand (ASTC.GetElementPtr True constantAddrStackPointer [ASTC.Int 32 0])) typeNodeNInteger [])
              tName <- emitInstr typeInt32Ptr (AST.GetElementPtr True nName [operandInt 32 0, operandInt 32 0] [])
              emitInstrVoid (AST.Store False (operandInt 32 1) tName Nothing 0 [])
              fName <- emitInstr typeInt32Ptr (AST.GetElementPtr True nName [operandInt 32 0, operandInt 32 1] [])
              emitInstrVoid (AST.Store False vName fName Nothing 0 [])

              -- Return
              -- __TODO: this is not correct. Exchange @asb@ with @asp@__
              emitTerm (AST.Ret Nothing [])
        )
      )
    ]

operandInt :: Word32 -> Integer -> AST.Operand
operandInt w n = AST.ConstantOperand (ASTC.Int w n)

constantAddrStackPointer :: ASTC.Constant
constantAddrStackPointer = ASTC.GlobalReference typeInt32PtrPtr "asp"

typeNodeNIntegerPtr :: ASTT.Type
typeNodeNIntegerPtr = ASTT.ptr typeNodeNInteger

typeNodeNInteger :: ASTT.Type
typeNodeNInteger = ASTT.NamedTypeReference "node.NInteger"

typeInt32PtrPtr :: ASTT.Type
typeInt32PtrPtr = ASTT.ptr typeInt32Ptr

typeInt32Ptr :: ASTT.Type
typeInt32Ptr = ASTT.ptr typeInt32

typeInt32 :: ASTT.Type
typeInt32 = ASTT.i32
