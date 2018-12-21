module Minicute.Common.Value where

data Value
  = IntegerValue Integer
  | ErrorValue
  deriving ( Eq, Show )
