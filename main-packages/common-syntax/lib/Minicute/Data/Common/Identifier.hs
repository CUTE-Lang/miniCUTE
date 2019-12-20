{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  showsPrec _ (Identifier v) = showString v
  {-# INLINE showsPrec #-}

makePrettyMCFromPretty ''Identifier
makeWrapped ''Identifier
