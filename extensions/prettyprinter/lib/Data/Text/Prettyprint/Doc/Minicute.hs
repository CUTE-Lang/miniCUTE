{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Copyright: (c) 2018-present Junyoung Clare Jang
-- License: BSD 3-Clause
--
-- Additional prettyprinter functions for miniCUTE compiler
module Data.Text.Prettyprint.Doc.Minicute
  ( PrettyMC( .. )
  , prettyMC0
  , prettyListMC0

  , prettyIndent
  , prettyWrappedIf

  , makePrettyMCFromPretty
  ) where

import Data.Foldable
import Data.Functor.Const
import Data.Functor.Identity
import Data.List.NonEmpty ( NonEmpty(..) )
import Data.Maybe
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Minicute.Internal.TH
import Data.Void
import GHC.Int
import GHC.Natural
import GHC.Word

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText

-- |
-- 'PrettyMC' (which stands for Pretty MiniCute) class is
-- a variation class of 'Pretty' for miniCUTE types,
-- which require a precedence to print prettily.
class PrettyMC a where
  {-# MINIMAL prettyMC #-}
  prettyMC :: Int -> a -> Doc ann
  prettyListMC :: Int -> [a] -> Doc ann
  prettyListMC p = align . list . fmap (prettyMC p)
  {-# INLINABLE prettyListMC #-}

prettyMC0 :: (PrettyMC a) => a -> Doc ann
prettyMC0 = prettyMC 0
{-# INLINE prettyMC0 #-}

prettyListMC0 :: (PrettyMC a) => [a] -> Doc ann
prettyListMC0 = prettyListMC 0
{-# INLINE prettyListMC0 #-}

fmap fold . traverse makePrettyMCFromPretty
  $ [ ''Bool
    , ''Char
    , ''Double
    , ''Float
    , ''Int
    , ''Int8
    , ''Int16
    , ''Int32
    , ''Int64
    , ''Integer
    , ''Natural
    , ''Word
    , ''Word8
    , ''Word16
    , ''Word32
    , ''Word64
    , ''()
    , ''Void
    , ''Text.Text
    , ''LazyText.Text
    ]

instance (PrettyMC a) => PrettyMC [a] where
  prettyMC = prettyListMC
  {-# INLINABLE prettyMC #-}

instance (PrettyMC a) => PrettyMC (Maybe a) where
  prettyMC p = foldMap (prettyMC p)
  {-# INLINABLE prettyMC #-}
  prettyListMC p = prettyListMC p . catMaybes
  {-# INLINABLE prettyListMC #-}

instance (PrettyMC a) => PrettyMC (Identity a) where
  prettyMC p = prettyMC p . runIdentity
  {-# INLINABLE prettyMC #-}

instance (PrettyMC a) => PrettyMC (NonEmpty a) where
  prettyMC p = prettyListMC p . NonEmpty.toList
  {-# INLINABLE prettyMC #-}

instance (PrettyMC a1, PrettyMC a2) => PrettyMC (a1, a2) where
  prettyMC p (x1, x2) = tupled [prettyMC p x1, prettyMC p x2]
  {-# INLINABLE prettyMC #-}

instance (PrettyMC a1, PrettyMC a2, PrettyMC a3) => PrettyMC (a1, a2, a3) where
  prettyMC p (x1, x2, x3) = tupled [prettyMC p x1, prettyMC p x2, prettyMC p x3]
  {-# INLINABLE prettyMC #-}

instance (PrettyMC a) => PrettyMC (Const a b) where
  prettyMC p = prettyMC p . getConst
  {-# INLINABLE prettyMC #-}


-- |
-- @prettyIndent doc@ make a document indented with an appropriate size.
prettyIndent :: Doc ann -> Doc ann
prettyIndent = indent 2
{-# INLINE prettyIndent #-}

prettyWrappedIf :: Bool -> (Doc ann -> Doc ann) -> Doc ann -> Doc ann
prettyWrappedIf True f = f
prettyWrappedIf False _ = id
{-# INLINABLE prettyWrappedIf #-}
