{- HLINT ignore "Use explicit module export list" -}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Copyright: (c) 2018-present Junyoung Clare Jang
-- License: BSD 3-Clause
module Minicute.Transpilers.LLVM.Constants
  ( module Minicute.Transpilers.LLVM.Constants
  ) where

import Data.String ( fromString )
import Data.Word
import Minicute.Data.Common

import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as ASTC
import qualified LLVM.AST.Type as ASTT

operandInt :: Word32 -> Int -> AST.Operand
operandInt w = AST.ConstantOperand . constantInt w
{-# INLINE operandInt #-}

operandInteger :: Word32 -> Integer -> AST.Operand
operandInteger w = AST.ConstantOperand . constantInteger w
{-# INLINE operandInteger #-}

operandUserDefinedNGlobal :: Identifier -> AST.Operand
operandUserDefinedNGlobal i = operandNGlobal $ "minicute__user_defined__" <> i <> "__node"
{-# INLINE operandUserDefinedNGlobal #-}

operandNGlobal :: Identifier -> AST.Operand
operandNGlobal (Identifier iStr)
  = AST.ConstantOperand . ASTC.GlobalReference typeNodeNGlobal $ fromString iStr
{-# INLINE operandNGlobal #-}

operandNodeCreateNInteger :: AST.Operand
operandNodeCreateNInteger = AST.ConstantOperand constantNodeCreateNInteger
{-# INLINE operandNodeCreateNInteger #-}

operandNodeCreateNStructure :: AST.Operand
operandNodeCreateNStructure = AST.ConstantOperand constantNodeCreateNStructure
{-# INLINE operandNodeCreateNStructure #-}

operandNodeCreateNStructureFields :: AST.Operand
operandNodeCreateNStructureFields = AST.ConstantOperand constantNodeCreateNStructureFields
{-# INLINE operandNodeCreateNStructureFields #-}

operandNodeCreateNApplication :: AST.Operand
operandNodeCreateNApplication = AST.ConstantOperand constantNodeCreateNApplication
{-# INLINE operandNodeCreateNApplication #-}

operandNodeUpdateNInteger :: AST.Operand
operandNodeUpdateNInteger = AST.ConstantOperand constantNodeUpdateNInteger
{-# INLINE operandNodeUpdateNInteger #-}

operandNodeUpdateNStructure :: AST.Operand
operandNodeUpdateNStructure = AST.ConstantOperand constantNodeUpdateNStructure
{-# INLINE operandNodeUpdateNStructure #-}

operandNodeUpdateNApplication :: AST.Operand
operandNodeUpdateNApplication = AST.ConstantOperand constantNodeUpdateNApplication
{-# INLINE operandNodeUpdateNApplication #-}

operandNodeUpdateNIndirect :: AST.Operand
operandNodeUpdateNIndirect = AST.ConstantOperand constantNodeUpdateNIndirect
{-# INLINE operandNodeUpdateNIndirect #-}

operandUtilUnwind :: AST.Operand
operandUtilUnwind = AST.ConstantOperand constantUtilUnwind
{-# INLINE operandUtilUnwind #-}

operandAddrStackPointer :: AST.Operand
operandAddrStackPointer = AST.ConstantOperand constantAddrStackPointer
{-# INLINE operandAddrStackPointer #-}

operandAddrBasePointer :: AST.Operand
operandAddrBasePointer = AST.ConstantOperand constantAddrBasePointer
{-# INLINE operandAddrBasePointer #-}

operandNodeHeapPointer :: AST.Operand
operandNodeHeapPointer = AST.ConstantOperand constantNodeHeapPointer
{-# INLINE operandNodeHeapPointer #-}

constantNodeCreateNInteger :: ASTC.Constant
constantNodeCreateNInteger = ASTC.GlobalReference typeNodeCreateNInteger "minicute__node__create_NInteger"
{-# INLINE constantNodeCreateNInteger #-}

constantNodeCreateNStructure :: ASTC.Constant
constantNodeCreateNStructure = ASTC.GlobalReference typeNodeCreateNStructure "minicute__node__create_NStructure"
{-# INLINE constantNodeCreateNStructure #-}

constantNodeCreateNStructureFields :: ASTC.Constant
constantNodeCreateNStructureFields = ASTC.GlobalReference typeNodeCreateNStructureFields "minicute__node__create_NStructureFields"
{-# INLINE constantNodeCreateNStructureFields #-}

constantNodeCreateNApplication :: ASTC.Constant
constantNodeCreateNApplication = ASTC.GlobalReference typeNodeCreateNApplication "minicute__node__create_NApplication"

constantNodeUpdateNInteger :: ASTC.Constant
constantNodeUpdateNInteger = ASTC.GlobalReference typeNodeUpdateNInteger "minicute__node__update_NInteger"
{-# INLINE constantNodeUpdateNInteger #-}

constantNodeUpdateNStructure :: ASTC.Constant
constantNodeUpdateNStructure = ASTC.GlobalReference typeNodeUpdateNStructure "minicute__node__update_NStructure"
{-# INLINE constantNodeUpdateNStructure #-}

constantNodeUpdateNApplication :: ASTC.Constant
constantNodeUpdateNApplication = ASTC.GlobalReference typeNodeUpdateNApplication "minicute__node__update_NApplication"
{-# INLINE constantNodeUpdateNApplication #-}

constantNodeUpdateNIndirect :: ASTC.Constant
constantNodeUpdateNIndirect = ASTC.GlobalReference typeNodeUpdateNIndirect "minicute__node__update_NIndirect"
{-# INLINE constantNodeUpdateNIndirect #-}

constantUtilUnwind :: ASTC.Constant
constantUtilUnwind = ASTC.GlobalReference typeUtilUnwind "minicute__util__unwind"
{-# INLINE constantUtilUnwind #-}

constantAddrStackPointer :: ASTC.Constant
constantAddrStackPointer = ASTC.GlobalReference typeInt8PtrPtrPtr "asp"
{-# INLINE constantAddrStackPointer #-}

constantAddrBasePointer :: ASTC.Constant
constantAddrBasePointer = ASTC.GlobalReference typeInt8PtrPtrPtr "abp"
{-# INLINE constantAddrBasePointer #-}

constantNodeHeapPointer :: ASTC.Constant
constantNodeHeapPointer = ASTC.GlobalReference typeInt8PtrPtr "nhp"
{-# INLINE constantNodeHeapPointer #-}

constantNodeNGlobal :: AST.Name -> ASTC.Constant
constantNodeNGlobal n
  = ASTC.Struct
    Nothing
    False
    [ constantInteger 8 6
    , ASTC.BitCast (ASTC.GlobalReference typeNodeCode n) typeInt8Ptr
    , constantInteger 32 0
    ]
{-# INLINE constantNodeNGlobal #-}

constantInt :: Word32 -> Int -> ASTC.Constant
constantInt w = constantInteger w . toInteger
{-# INLINE constantInt #-}

constantInteger :: Word32 -> Integer -> ASTC.Constant
constantInteger = ASTC.Int
{-# INLINE constantInteger #-}

typeNodeCreateNInteger :: ASTT.Type
typeNodeCreateNInteger = ASTT.FunctionType typeInt8Ptr [typeInt32] False
{-# INLINE typeNodeCreateNInteger #-}

typeNodeCreateNStructure :: ASTT.Type
typeNodeCreateNStructure = ASTT.FunctionType typeInt8Ptr [typeInt32, typeInt8Ptr] False
{-# INLINE typeNodeCreateNStructure #-}

typeNodeCreateNStructureFields :: ASTT.Type
typeNodeCreateNStructureFields = ASTT.FunctionType typeInt8Ptr [typeInt32, typeInt8PtrPtr] False
{-# INLINE typeNodeCreateNStructureFields #-}

typeNodeCreateNApplication :: ASTT.Type
typeNodeCreateNApplication = ASTT.FunctionType typeInt8Ptr [typeInt8Ptr, typeInt8Ptr] False
{-# INLINE typeNodeCreateNApplication #-}

typeNodeUpdateNInteger :: ASTT.Type
typeNodeUpdateNInteger = ASTT.FunctionType ASTT.void [typeInt32, typeInt8Ptr] False
{-# INLINE typeNodeUpdateNInteger #-}

typeNodeUpdateNStructure :: ASTT.Type
typeNodeUpdateNStructure = ASTT.FunctionType ASTT.void [typeInt32, typeInt8Ptr, typeInt8Ptr] False
{-# INLINE typeNodeUpdateNStructure #-}

typeNodeUpdateNApplication :: ASTT.Type
typeNodeUpdateNApplication = ASTT.FunctionType ASTT.void [typeInt8Ptr, typeInt8Ptr, typeInt8Ptr] False
{-# INLINE typeNodeUpdateNApplication #-}

typeNodeUpdateNIndirect :: ASTT.Type
typeNodeUpdateNIndirect = ASTT.FunctionType ASTT.void [typeInt8Ptr, typeInt8Ptr] False
{-# INLINE typeNodeUpdateNIndirect #-}

typeUtilUnwind :: ASTT.Type
typeUtilUnwind = ASTT.FunctionType ASTT.void [] False
{-# INLINE typeUtilUnwind #-}

typeNodeCode :: ASTT.Type
typeNodeCode = ASTT.FunctionType ASTT.void [] False
{-# INLINE typeNodeCode #-}

typeNodeNIntegerPtr :: ASTT.Type
typeNodeNIntegerPtr = ASTT.ptr typeNodeNInteger
{-# INLINE typeNodeNIntegerPtr #-}

typeNodeNInteger :: ASTT.Type
typeNodeNInteger = ASTT.NamedTypeReference "node.NInteger"
{-# INLINE typeNodeNInteger #-}

typeNodeNGlobal :: ASTT.Type
typeNodeNGlobal = ASTT.NamedTypeReference "node.NGlobal"
{-# INLINE typeNodeNGlobal #-}

typeInt8PtrPtrPtr :: ASTT.Type
typeInt8PtrPtrPtr = ASTT.ptr typeInt8PtrPtr
{-# INLINE typeInt8PtrPtrPtr #-}

typeInt8PtrPtr :: ASTT.Type
typeInt8PtrPtr = ASTT.ptr typeInt8Ptr
{-# INLINE typeInt8PtrPtr #-}

typeInt8Ptr :: ASTT.Type
typeInt8Ptr = ASTT.ptr typeInt8
{-# INLINE typeInt8Ptr #-}

typeInt8 :: ASTT.Type
typeInt8 = ASTT.i8
{-# INLINE typeInt8 #-}

typeInt32PtrPtrPtr :: ASTT.Type
typeInt32PtrPtrPtr = ASTT.ptr typeInt32PtrPtr
{-# INLINE typeInt32PtrPtrPtr #-}

typeInt32PtrPtr :: ASTT.Type
typeInt32PtrPtr = ASTT.ptr typeInt32Ptr
{-# INLINE typeInt32PtrPtr #-}

typeInt32Ptr :: ASTT.Type
typeInt32Ptr = ASTT.ptr typeInt32
{-# INLINE typeInt32Ptr #-}

typeInt32 :: ASTT.Type
typeInt32 = ASTT.i32
{-# INLINE typeInt32 #-}
