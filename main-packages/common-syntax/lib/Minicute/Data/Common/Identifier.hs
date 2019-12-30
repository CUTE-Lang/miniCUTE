{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- Copyright: (c) 2018-present Junyoung Clare Jang
-- License: BSD 3-Clause
--
-- Common basic types used to define many other types
module Minicute.Data.Common.Identifier
  ( Identifier( .. )
  , identifierLength
  ) where

import Data.Data ( Data, Typeable )
import Data.String ( IsString(..) )
import Data.Text.Prettyprint.Doc ( Pretty(..) )
import Data.Text.Prettyprint.Doc.Minicute
import GHC.Generics ( Generic )
import Language.Haskell.TH.Syntax ( Lift )

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
           , IsString
           , Semigroup
           , Pretty
           )

instance Show Identifier where
  showsPrec p (Identifier v) = showsPrec p v
  {-# INLINE showsPrec #-}

makePrettyMCFromPretty ''Identifier

-- |
-- get the length of 'Identifier'
identifierLength :: Identifier -> Int
identifierLength (Identifier v) = length v
{-# INLINE identifierLength #-}
