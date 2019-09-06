{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Reduce duplication" -}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
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

    , ( "a program with an integer supercombinator"
      , [qqGMachine|
           f<0> {
             PushBasicValue 100;
             UpdateAsInteger 0;
             Return;
           }
        |]
      , execModuleBuilder emptyModuleBuilder $ do
          function "minicute__user__defined__f" [] ASTT.void . const $ do
            emitBlockStart "entry"

            -- PushBasicValue 100
            pName <- alloca ASTT.i32 Nothing 0
            store (operandInt 32 100) 0 pName
            vName <- load pName 0

            -- UpdateAsInteger 0
            sName <- load operandAddrStackPointer 0
            sName' <- gep sName [operandInteger 32 0]
            nName <- load sName' 0
            _ <- call operandUpdateNodeNInteger [(vName, []), (nName, [])]

            -- Return
            bName <- load operandAddrBasePointer 0
            bName' <- gep bName [operandInteger 32 0]
            store bName' 0 operandAddrStackPointer
            retVoid
      )

    , ( "a program with a structure supercombinator"
      , [qqGMachine|
           f<0> {
             PushBasicValue 1;
             UpdateAsStructure 0;
             Return;
           }
        |]
      , execModuleBuilder emptyModuleBuilder $ do
          function "minicute__user__defined__f" [] ASTT.void . const $ do
            emitBlockStart "entry"

            -- PushBasicValue 1
            pName <- alloca ASTT.i32 Nothing 0
            store (operandInt 32 1) 0 pName
            vName <- load pName 0

            -- UpdateAsStructure 0
            sName <- load operandAddrStackPointer 0
            sName' <- gep sName [operandInteger 32 0]
            nName <- load sName' 0
            fName <- alloca (ASTT.ArrayType 0 typeInt8Ptr) Nothing 0
            fName' <- gep fName [operandInteger 32 0, operandInteger 32 0]
            fName'' <- call operandCreateNodeNStructureFields [(operandInteger 32 0, []), (fName', [])]
            _ <- call operandUpdateNodeNStructure [(vName, []), (fName'', []), (nName, [])]

            -- Return
            bName <- load operandAddrBasePointer 0
            bName' <- gep bName [operandInteger 32 0]
            store bName' 0 operandAddrStackPointer
            retVoid
      )

    , ( "a program with an alias supercombinator"
      , [qqGMachine|
           f<0> {
             MakeGlobal g;
             Eval;
             Update 1;
             Pop 1;
             Unwind;
           }
        |]
      , execModuleBuilder emptyModuleBuilder $ do
          function "minicute__user__defined__f" [] ASTT.void . const $ do
            emitBlockStart "entry"

            -- MakeGlobal g
            sName <- load operandAddrStackPointer 0
            sName' <- gep sName [operandInteger 32 1]
            nName <- bitcast (operandNGlobal "minicute__user__defined__g") typeInt8Ptr
            store nName 0 sName'
            store sName' 0 operandAddrStackPointer

            -- Eval
            bName <- load operandAddrBasePointer 0
            sName <- load operandAddrStackPointer 0
            sName' <- gep sName [operandInteger 32 (negate 1)]
            store sName' 0 operandAddrBasePointer
            _ <- call operandUtilUnwind []
            store bName 0 operandAddrBasePointer

            -- Update 1
            sName <- load operandAddrStackPointer 0
            sName' <- gep sName [operandInteger 32 0]
            nName <- load sName' 0
            sName'' <- gep sName [operandInteger 32 (negate 1)]
            nName' <- load sName'' 0
            _ <- call operandUpdateNodeNIndirect [(nName, []), (nName', [])]

            -- Pop 1
            sName <- load operandAddrStackPointer 0
            sName' <- gep sName [operandInteger 32 (negate 1)]
            store sName' 0 operandAddrStackPointer

            -- Unwind
            _ <- call operandUtilUnwind []
            retVoid
      )

    , ( "a program with a single-argument supercombinator"
      , [qqGMachine|
           f<1> {
             Copy 0;
             Eval;
             Update 2;
             Pop 2;
             Unwind;
           }
        |]
      , execModuleBuilder emptyModuleBuilder $ do
          function "minicute__user__defined__f" [] ASTT.void . const $ do
            emitBlockStart "entry"

            -- Copy 0
            sName <- load operandAddrStackPointer 0
            sName' <- gep sName [operandInteger 32 0]
            nName <- load sName' 0
            sName'' <- gep sName [operandInteger 32 1]
            store nName 0 sName''
            store sName'' 0 operandAddrStackPointer

            -- Eval
            bName <- load operandAddrBasePointer 0
            sName <- load operandAddrStackPointer 0
            sName' <- gep sName [operandInteger 32 (negate 1)]
            store sName' 0 operandAddrBasePointer
            _ <- call operandUtilUnwind []
            store bName 0 operandAddrBasePointer

            -- Update 2
            sName <- load operandAddrStackPointer 0
            sName' <- gep sName [operandInteger 32 0]
            nName <- load sName' 0
            sName'' <- gep sName [operandInteger 32 (negate 2)]
            nName' <- load sName'' 0
            _ <- call operandUpdateNodeNIndirect [(nName, []), (nName', [])]

            -- Pop 2
            sName <- load operandAddrStackPointer 0
            sName' <- gep sName [operandInteger 32 (negate 2)]
            store sName' 0 operandAddrStackPointer

            -- Unwind
            _ <- call operandUtilUnwind []
            retVoid
      )

    , ( "a program with a supercombinator of an application"
      , [qqGMachine|
           f<0> {
             MakeGlobal g;
             MakeInteger 0;
             MakeApplication;
             Eval;
             Update 1;
             Pop 1;
             Unwind;
           }
        |]
      , execModuleBuilder emptyModuleBuilder $ do
          function "minicute__user__defined__f" [] ASTT.void . const $ do
            emitBlockStart "entry"

            -- MakeGlobal g
            sName <- load operandAddrStackPointer 0
            sName' <- gep sName [operandInteger 32 1]
            nName <- bitcast (operandNGlobal "minicute__user__defined__g") typeInt8Ptr
            store nName 0 sName'
            store sName' 0 operandAddrStackPointer

            -- MakeInteger 0
            sName <- load operandAddrStackPointer 0
            sName' <- gep sName [operandInteger 32 1]
            nName <- call operandCreateNodeNInteger [(operandInteger 32 0, [])]
            store nName 0 sName'
            store sName' 0 operandAddrStackPointer

            -- MakeApplication
            sName <- load operandAddrStackPointer 0
            sName' <- gep sName [operandInteger 32 (negate 1)]
            fName <- load sName' 0
            sName'' <- gep sName [operandInteger 32 0]
            aName <- load sName'' 0
            nName <- call operandCreateNodeNApplication [(fName, []), (aName, [])]
            store nName 0 sName'
            store sName' 0 operandAddrStackPointer

            -- Eval
            bName <- load operandAddrBasePointer 0
            sName <- load operandAddrStackPointer 0
            sName' <- gep sName [operandInteger 32 (negate 1)]
            store sName' 0 operandAddrBasePointer
            _ <- call operandUtilUnwind []
            store bName 0 operandAddrBasePointer

            -- Update 1
            sName <- load operandAddrStackPointer 0
            sName' <- gep sName [operandInteger 32 0]
            nName <- load sName' 0
            sName'' <- gep sName [operandInteger 32 (negate 1)]
            nName' <- load sName'' 0
            _ <- call operandUpdateNodeNIndirect [(nName, []), (nName', [])]

            -- Pop 1
            sName <- load operandAddrStackPointer 0
            sName' <- gep sName [operandInteger 32 (negate 1)]
            store sName' 0 operandAddrStackPointer

            -- Unwind
            _ <- call operandUtilUnwind []
            retVoid
      )
    ]
