{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Additional prettyprinter functions for miniCUTE compiler
module Data.Text.Prettyprint.Doc.Minicute where

import Data.Text.Prettyprint.Doc

import qualified Data.Text.Prettyprint.Doc.Render.String as PPS

-- |
-- 'PrettyPrec' class is a variation class of 'Pretty' for types that require precedence to print prettily.
class (Pretty a) => PrettyPrec a where
  prettyPrec :: Int -> a -> Doc ann
  prettyPrec _ = pretty
  {-# INLINABLE prettyPrec #-}

  prettyPrec0 :: a -> Doc ann
  prettyPrec0 = prettyPrec 0
  {-# INLINABLE prettyPrec0 #-}

-- |
-- A function to show input prettily.
prettyShow :: (Pretty a) => a -> String
prettyShow = PPS.renderString . layoutPretty defaultLayoutOptions . pretty
{-# INLINABLE prettyShow #-}
