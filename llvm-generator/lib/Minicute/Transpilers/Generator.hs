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
    go vStack (IPushBasicValue v : insts@(_ : _)) = do
      pName <- alloca ASTT.i32 Nothing 0
      store (operandInt 32 v) 0 pName
      vName <- load pName 0

      go (vName : vStack) insts
    go (vName : vStack) (IUpdateAsInteger n : insts@(_ : _)) = do
      sName <- load (AST.ConstantOperand constantAddrStackPointer) 0
      sName' <- gep sName [operandInt 32 (toInteger (negate n))]
      nName <- load sName' 0
      _ <- call (AST.ConstantOperand constantUpdateNodeNInteger) [(vName, []), (nName, [])]

      go vStack insts
    go _ [IReturn] = do
      bName <- load (AST.ConstantOperand constantAddrBasePointer) 0
      bName' <- gep bName [operandInt 32 0]
      store bName' 0 (AST.ConstantOperand constantAddrStackPointer)
      retVoid
    go _ _ = error "generateMachineCodeE: Not yet implemented"
