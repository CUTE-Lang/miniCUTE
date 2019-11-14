{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
module Minicute.Data.Common.ExpressionLevel
  ( ExpressionLevel( .. )
  ) where

import Data.Data ( Data, Typeable )
import GHC.Generics ( Generic )
import Language.Haskell.TH.Syntax ( Lift )

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
