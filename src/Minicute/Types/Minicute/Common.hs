{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE PatternSynonyms #-}
module Minicute.Types.Minicute.Common
  ( Identifier


  , IsRecursive( .. )
  , pattern Recursive
  , pattern NonRecursive
  ) where

import Data.Data
import GHC.Generics
import Language.Haskell.TH.Syntax

type Identifier = String


newtype IsRecursive = IsRecursive { isRecursive :: Bool }
  deriving ( Generic
           , Typeable
           , Data
           , Lift
           , Eq
           , Ord
           )
pattern Recursive :: IsRecursive
pattern Recursive = IsRecursive True
pattern NonRecursive :: IsRecursive
pattern NonRecursive = IsRecursive False
{-# COMPLETE Recursive, NonRecursive #-}

instance Show IsRecursive where
  showsPrec _ Recursive = showString "Recursive"
  showsPrec _ NonRecursive = showString "NonRecursive"
