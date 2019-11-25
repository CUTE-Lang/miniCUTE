{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Copyright: (c) 2018-present Junyoung Clare Jang
-- License: BSD 3-Clause
--
-- Common basic types used to define many other types
module Minicute.Data.Common.Identifier
  ( Identifier( .. )
  ) where

import Control.Lens.TH
import Data.Data ( Data, Typeable )
import Data.String ( IsString(..) )
import Data.Text.Prettyprint.Doc.Minicute
import GHC.Generics ( Generic )
import Language.Haskell.TH.Syntax ( Lift )

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

makeWrapped ''Identifier
