{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Text.Prettyprint.Doc.Minicute where

import Data.Text.Prettyprint.Doc

import qualified Data.Text.Prettyprint.Doc.Render.String as PPS

class (Pretty a) => PrettyPrec a where
  prettyPrec :: Int -> a -> Doc ann
  prettyPrec _ = pretty
  {-# INLINABLE prettyPrec #-}

  prettyPrec0 :: a -> Doc ann
  prettyPrec0 = prettyPrec 0
  {-# INLINABLE prettyPrec0 #-}

prettyShow :: (Pretty a) => a -> String
prettyShow = PPS.renderString . layoutPretty defaultLayoutOptions . pretty
{-# INLINABLE prettyShow #-}
