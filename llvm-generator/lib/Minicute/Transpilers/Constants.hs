{- HLINT ignore "Use explicit module export list" -}
{-# LANGUAGE OverloadedStrings #-}
module Minicute.Transpilers.Constants
  ( module Minicute.Transpilers.Constants
  ) where

import Data.Word

import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as ASTC
import qualified LLVM.AST.Type as ASTT

operandInt :: Word32 -> Int -> AST.Operand
operandInt w n = AST.ConstantOperand (ASTC.Int w (toInteger n))

operandInteger :: Word32 -> Integer -> AST.Operand
operandInteger w n = AST.ConstantOperand (ASTC.Int w n)

operandNGlobal :: AST.Name -> AST.Operand
operandNGlobal = AST.ConstantOperand . ASTC.GlobalReference typeNodeNGlobal

constantCreateNodeNInteger :: ASTC.Constant
constantCreateNodeNInteger = ASTC.GlobalReference typeCreateNodeNInteger "minicute_create_node_NInteger"

constantCreateNodeNApplication :: ASTC.Constant
constantCreateNodeNApplication = ASTC.GlobalReference typeCreateNodeNApplication "minicute_create_node_NApplication"

constantUpdateNodeNInteger :: ASTC.Constant
constantUpdateNodeNInteger = ASTC.GlobalReference typeUpdateNodeNInteger "minicute_update_node_NInteger"

constantUpdateNodeNIndirect :: ASTC.Constant
constantUpdateNodeNIndirect = ASTC.GlobalReference typeUpdateNodeNIndirect "minicute_update_node_NIndirect"

constantUtilUnwind :: ASTC.Constant
constantUtilUnwind = ASTC.GlobalReference typeUtilUnwind "minicute__util__unwind"

constantAddrStackPointer :: ASTC.Constant
constantAddrStackPointer = ASTC.GlobalReference typeInt8PtrPtrPtr "asp"

constantAddrBasePointer :: ASTC.Constant
constantAddrBasePointer = ASTC.GlobalReference typeInt8PtrPtrPtr "abp"

constantNodeHeapPointer :: ASTC.Constant
constantNodeHeapPointer = ASTC.GlobalReference typeInt8PtrPtr "nhp"

typeCreateNodeNInteger :: ASTT.Type
typeCreateNodeNInteger = ASTT.FunctionType typeInt8Ptr [typeInt32] False

typeCreateNodeNApplication :: ASTT.Type
typeCreateNodeNApplication = ASTT.FunctionType typeInt8Ptr [typeInt8Ptr, typeInt8Ptr] False

typeUpdateNodeNInteger :: ASTT.Type
typeUpdateNodeNInteger = ASTT.FunctionType ASTT.void [typeInt32, typeInt8Ptr] False

typeUpdateNodeNIndirect :: ASTT.Type
typeUpdateNodeNIndirect = ASTT.FunctionType ASTT.void [typeInt8Ptr, typeInt8Ptr] False

typeUtilUnwind :: ASTT.Type
typeUtilUnwind = ASTT.FunctionType ASTT.void [] False

typeNodeNIntegerPtr :: ASTT.Type
typeNodeNIntegerPtr = ASTT.ptr typeNodeNInteger

typeNodeNInteger :: ASTT.Type
typeNodeNInteger = ASTT.NamedTypeReference "node.NInteger"

typeNodeNGlobal :: ASTT.Type
typeNodeNGlobal = ASTT.NamedTypeReference "node.NGlobal"

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
