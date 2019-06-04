{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Minicute.Types.Minicute.Common
  ( Identifier( .. )


  , IsRecursive( .. )
  , pattern Recursive
  , pattern NonRecursive
  ) where

import Control.Lens.TH
import Data.Data
import Data.String ( IsString(..) )
import Data.Text.Prettyprint.Doc ( Pretty(..) )
import GHC.Generics
import Language.Haskell.TH.Syntax

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

instance Pretty Identifier where
  pretty (Identifier v) = pretty v

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
  {-# INLINABLE showsPrec #-}

makeWrapped ''Identifier

makeWrapped ''IsRecursive
