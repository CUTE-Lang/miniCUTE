{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE OverloadedStrings #-}
module Minicute.Data.Common.Primitive
  ( module Minicute.Data.Common.Precedence

  , Primitive( .. )

  , toString

  , primitivePrecedenceTable
  , binaryPrimitivePrecedenceTable
  ) where

import Data.Data ( Data, Typeable )
import Data.String
import Data.Text.Prettyprint.Doc.Minicute ( PrettyMC(..) )
import GHC.Generics ( Generic )
import Language.Haskell.TH.Syntax ( Lift )
import Minicute.Data.Common.Precedence

data Primitive
  = PrimAdd
  | PrimSub
  | PrimMul
  | PrimDiv
  | PrimEq
  | PrimNe
  | PrimLt
  | PrimLe
  | PrimGt
  | PrimGe
  deriving ( Generic
           , Typeable
           , Data
           , Lift
           , Eq
           , Ord
           , Show
           , Read
           )

toString :: Primitive -> String
toString PrimAdd = "+"
toString PrimSub = "-"
toString PrimMul = "*"
toString PrimDiv = "/"
toString PrimEq = "=="
toString PrimNe = "/="
toString PrimLt = "<"
toString PrimLe = "<="
toString PrimGt = ">"
toString PrimGe = ">="

instance PrettyMC Primitive where
  prettyMC _ = fromString . toString

-- |
-- All predefined precedences.
--
-- All precedences should be smaller than 'miniApplicationPrecedence'.
-- Where do I need to check this condition?
primitivePrecedenceTable :: PrecedenceTable Primitive
primitivePrecedenceTable
  = binaryPrimitivePrecedenceTable

-- |
-- All precedences of binary primitives.
binaryPrimitivePrecedenceTable :: PrecedenceTable Primitive
binaryPrimitivePrecedenceTable
  = [ (PrimGe, PInfixL 10)
    , (PrimGt, PInfixL 10)
    , (PrimLe, PInfixL 10)
    , (PrimLt, PInfixL 10)
    , (PrimEq, PInfixL 10)
    , (PrimNe, PInfixL 10)
    , (PrimAdd, PInfixL 40)
    , (PrimSub, PInfixL 40)
    , (PrimMul, PInfixL 50)
    , (PrimDiv, PInfixL 50)
    ]
