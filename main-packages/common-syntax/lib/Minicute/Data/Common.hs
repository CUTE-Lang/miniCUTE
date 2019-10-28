{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Common basic types used to define many other types
module Minicute.Data.Common
  ( Identifier( .. )


  , IsRecursive( .. )
  , pattern Recursive
  , pattern NonRecursive


  , ExpressionLevel( .. )
  ) where

import Control.Lens.TH
import Data.Data
import Data.String ( IsString(..) )
import Data.Text.Prettyprint.Doc.Minicute
import GHC.Generics
import Language.Haskell.TH.Syntax

import qualified Data.Text.Prettyprint.Doc as PP

-- |
-- miniCUTE identifier type.
newtype Identifier
  = Identifier String
  deriving ( Generic
           , Typeable
           , Data
           , Lift
           , Eq
           , Ord
           )

instance Show Identifier where
  showsPrec _ (Identifier v) = showString v

instance IsString Identifier where
  fromString = Identifier

instance Semigroup Identifier where
  (Identifier a) <> (Identifier b) = Identifier (a <> b)

instance PP.Pretty Identifier where
  pretty (Identifier v) = PP.pretty v

instance PrettyMC Identifier where
  prettyMC _ = PP.pretty


-- |
-- @IsRecursive@ represents recursiveness of let/letrec expressions.
newtype IsRecursive = IsRecursive { isRecursive :: Bool }
  deriving ( Generic
           , Typeable
           , Data
           , Lift
           , Eq
           , Ord
           )
-- |
-- Utility pattern for the recursive case of 'IsRecursive'
pattern Recursive :: IsRecursive
pattern Recursive = IsRecursive True
-- |
-- Utility pattern for the non-recursive case of 'IsRecursive'
pattern NonRecursive :: IsRecursive
pattern NonRecursive = IsRecursive False
{-# COMPLETE Recursive, NonRecursive #-}

instance Show IsRecursive where
  showsPrec _ Recursive = showString "Recursive"
  showsPrec _ NonRecursive = showString "NonRecursive"
  {-# INLINABLE showsPrec #-}


data ExpressionLevel
  = MC -- ^ miniCUTE
  | LLMC -- ^ Lambda lifted miniCUTE
  deriving ( Generic
           , Typeable
           , Data
           , Lift
           , Eq
           , Ord
           , Show
           )


makeWrapped ''Identifier

makeWrapped ''IsRecursive
