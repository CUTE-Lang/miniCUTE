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

operandInt :: Word32 -> Integer -> AST.Operand
operandInt w n = AST.ConstantOperand (ASTC.Int w n)

constantUpdateNodeNInteger :: ASTC.Constant
constantUpdateNodeNInteger = ASTC.GlobalReference typeUpdateNodeNInteger "minicute_update_node_NInteger"

constantAddrStackPointer :: ASTC.Constant
constantAddrStackPointer = ASTC.GlobalReference typeInt8PtrPtrPtr "asp"

constantAddrBasePointer :: ASTC.Constant
constantAddrBasePointer = ASTC.GlobalReference typeInt8PtrPtrPtr "abp"

constantNodeHeapPointer :: ASTC.Constant
constantNodeHeapPointer = ASTC.GlobalReference typeInt8PtrPtr "nhp"

typeUpdateNodeNInteger :: ASTT.Type
typeUpdateNodeNInteger = ASTT.FunctionType ASTT.void [typeInt32, typeInt8Ptr] False

typeNodeNIntegerPtr :: ASTT.Type
typeNodeNIntegerPtr = ASTT.ptr typeNodeNInteger

typeNodeNInteger :: ASTT.Type
typeNodeNInteger = ASTT.NamedTypeReference "node.NInteger"

typeInt8PtrPtrPtr :: ASTT.Type
typeInt8PtrPtrPtr = ASTT.ptr typeInt8PtrPtr

typeInt8PtrPtr :: ASTT.Type
typeInt8PtrPtr = ASTT.ptr typeInt8Ptr

typeInt8Ptr :: ASTT.Type
typeInt8Ptr = ASTT.ptr typeInt8

typeInt8 :: ASTT.Type
typeInt8 = ASTT.i8

typeInt32PtrPtrPtr :: ASTT.Type
typeInt32PtrPtrPtr = ASTT.ptr typeInt32PtrPtr

typeInt32PtrPtr :: ASTT.Type
typeInt32PtrPtr = ASTT.ptr typeInt32Ptr

typeInt32Ptr :: ASTT.Type
typeInt32Ptr = ASTT.ptr typeInt32

typeInt32 :: ASTT.Type
typeInt32 = ASTT.i32
