{- HLINT ignore "Reduce duplication" -}
{-# LANGUAGE OverloadedStrings #-}
module Minicute.Transpilers.Generator
  ( module Minicute.Data.GMachine.Instruction
  , generateMachineCode
  ) where

import Control.Monad
import Data.String
import LLVM.IRBuilder
import Minicute.Data.GMachine.Instruction
import Minicute.Data.Minicute.Common
import Minicute.Transpilers.Constants

import qualified LLVM.AST as AST
import qualified LLVM.AST.Type as ASTT

generateMachineCode :: GMachineProgram -> [AST.Definition]
generateMachineCode program = execModuleBuilder emptyModuleBuilder (generateMachineCodeProgram program)

generateMachineCodeProgram :: GMachineProgram -> ModuleBuilder ()
generateMachineCodeProgram program = forM_ program generateMachineCodeSc

generateMachineCodeSc :: GMachineSupercombinator -> ModuleBuilder ()
generateMachineCodeSc (Identifier binder, _, expr)
  = void . function functionName [] ASTT.void . const $ bodyBuilder
  where
    functionName = fromString ("minicute__user__defined__" <> binder)
    bodyBuilder = do
      emitBlockStart "entry"
      generateMachineCodeE expr

generateMachineCodeE :: GMachineExpression -> IRBuilderT ModuleBuilder ()
generateMachineCodeE = go []
  where
    go vStack (IMakeInteger v : insts@(_ : _)) = do
      sName <- load (AST.ConstantOperand constantAddrStackPointer) 0
      sName' <- gep sName [operandInteger 32 1]
      nName <- call (AST.ConstantOperand constantCreateNodeNInteger) [(operandInteger 32 v, [])]
      store nName 0 sName'
      store sName' 0 (AST.ConstantOperand constantAddrStackPointer)

      go vStack insts
    go vStack (IMakeApplication : insts@(_ : _)) = do
      sName <- load (AST.ConstantOperand constantAddrStackPointer) 0
      sName' <- gep sName [operandInteger 32 (negate 1)]
      fName <- load sName' 0
      sName'' <- gep sName [operandInteger 32 0]
      aName <- load sName'' 0
      nName <- call (AST.ConstantOperand constantCreateNodeNApplication) [(fName, []), (aName, [])]
      store nName 0 sName'
      store sName' 0 (AST.ConstantOperand constantAddrStackPointer)

      go vStack insts
    go vStack (IMakeGlobal i : insts@(_ : _)) = do
      sName <- load (AST.ConstantOperand constantAddrStackPointer) 0
      sName' <- gep sName [operandInteger 32 1]
      nName <- bitcast (operandUserDefinedNGlobal i) typeInt8Ptr
      store nName 0 sName'
      store sName' 0 (AST.ConstantOperand constantAddrStackPointer)

      go vStack insts

    go vStack (IPop n : insts@(_ : _)) = do
      sName <- load (AST.ConstantOperand constantAddrStackPointer) 0
      sName' <- gep sName [operandInt 32 (negate n)]
      store sName' 0 (AST.ConstantOperand constantAddrStackPointer)

      go vStack insts
    go vStack (IUpdate n : insts@(_ : _)) = do
      sName <- load (AST.ConstantOperand constantAddrStackPointer) 0
      sName' <- gep sName [operandInteger 32 0]
      nName <- load sName' 0
      sName'' <- gep sName [operandInt 32 (negate n)]
      nName' <- load sName'' 0
      _ <- call (AST.ConstantOperand constantUpdateNodeNIndirect) [(nName, []), (nName', [])]

      go vStack insts
    go vStack (ICopy n : insts@(_ : _)) = do
      sName <- load (AST.ConstantOperand constantAddrStackPointer) 0
      sName' <- gep sName [operandInt 32 (negate n)]
      nName <- load sName' 0
      sName'' <- gep sName [operandInteger 32 1]
      store nName 0 sName''
      store sName'' 0 (AST.ConstantOperand constantAddrStackPointer)

      go vStack insts

    go vStack (IPushBasicValue v : insts@(_ : _)) = do
      pName <- alloca ASTT.i32 Nothing 0
      store (operandInteger 32 v) 0 pName
      vName <- load pName 0

      go (vName : vStack) insts
    go (vName : vStack) (IUpdateAsInteger n : insts@(_ : _)) = do
      sName <- load (AST.ConstantOperand constantAddrStackPointer) 0
      sName' <- gep sName [operandInt 32 (negate n)]
      nName <- load sName' 0
      _ <- call (AST.ConstantOperand constantUpdateNodeNInteger) [(vName, []), (nName, [])]

      go vStack insts

    go _ [IUnwind] = do
      _ <- call (AST.ConstantOperand constantUtilUnwind) []

      retVoid

    go vStack (IEval : insts@(_ : _)) = do
      evalBody

      go vStack insts
    go _ [IEval] = do
      evalBody

      retVoid
    go _ [IReturn] = do
      bName <- load (AST.ConstantOperand constantAddrBasePointer) 0
      bName' <- gep bName [operandInteger 32 0]
      store bName' 0 (AST.ConstantOperand constantAddrStackPointer)

      retVoid

    go _ _ = error "generateMachineCodeE: Not yet implemented"

    evalBody = do
      bName <- load (AST.ConstantOperand constantAddrBasePointer) 0
      sName <- load (AST.ConstantOperand constantAddrStackPointer) 0
      sName' <- gep sName [operandInteger 32 (negate 1)]
      store sName' 0 (AST.ConstantOperand constantAddrBasePointer)
      _ <- call (AST.ConstantOperand constantUtilUnwind) []
      store bName 0 (AST.ConstantOperand constantAddrBasePointer)
