{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Minicute.Data.Common.IsRecursive
  ( IsRecursive( .. )
  , pattern Recursive
  , pattern NonRecursive
  ) where

import Control.Lens.TH
import Data.Data ( Data, Typeable )
import GHC.Generics ( Generic )
import Language.Haskell.TH.Syntax ( Lift )

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

makeWrapped ''IsRecursive
