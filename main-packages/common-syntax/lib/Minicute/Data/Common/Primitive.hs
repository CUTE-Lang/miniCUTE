{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE OverloadedStrings #-}
module Minicute.Data.Common.Primitive
  ( Primitive( .. )
  ) where

import Data.Data ( Data, Typeable )
import Data.Text.Prettyprint.Doc.Minicute ( PrettyMC(..) )
import GHC.Generics ( Generic )
import Language.Haskell.TH.Syntax ( Lift )

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

instance PrettyMC Primitive where
  prettyMC _ PrimAdd = "+"
  prettyMC _ PrimSub = "-"
  prettyMC _ PrimMul = "*"
  prettyMC _ PrimDiv = "/"
  prettyMC _ PrimEq = "=="
  prettyMC _ PrimNe = "/="
  prettyMC _ PrimLt = "<"
  prettyMC _ PrimLe = "<="
  prettyMC _ PrimGt = ">"
  prettyMC _ PrimGe = ">="
