module Minicute.Common.Program
  ( module Minicute.Common.Expression
  , Program( .. )
  ) where

import Minicute.Common.Expression

newtype Program
  = Program Expression
  deriving ( Eq, Show )
