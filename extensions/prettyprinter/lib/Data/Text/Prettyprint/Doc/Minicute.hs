{-# LANGUAGE FlexibleInstances #-}
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
  ) where

import Data.Functor.Const
import Data.Functor.Identity
import Data.List.NonEmpty ( NonEmpty(..) )
import Data.Maybe
import Data.Text.Prettyprint.Doc
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
{-# INLINABLE prettyMC0 #-}

prettyListMC0 :: (PrettyMC a) => [a] -> Doc ann
prettyListMC0 = prettyListMC 0
{-# INLINABLE prettyListMC0 #-}

-- Replace the following instances by
-- TH derived instances
--  or
-- Introduce default method implementation

instance PrettyMC Bool where
  prettyMC _ = pretty
  {-# INLINABLE prettyMC #-}
  prettyListMC _ = prettyList
  {-# INLINABLE prettyListMC #-}

instance PrettyMC Char where
  prettyMC _ = pretty
  {-# INLINABLE prettyMC #-}
  prettyListMC _ = prettyList
  {-# INLINABLE prettyListMC #-}

instance PrettyMC Double where
  prettyMC _ = pretty
  {-# INLINABLE prettyMC #-}
  prettyListMC _ = prettyList
  {-# INLINABLE prettyListMC #-}

instance PrettyMC Float where
  prettyMC _ = pretty
  {-# INLINABLE prettyMC #-}
  prettyListMC _ = prettyList
  {-# INLINABLE prettyListMC #-}

instance PrettyMC Int where
  prettyMC _ = pretty
  {-# INLINABLE prettyMC #-}
  prettyListMC _ = prettyList
  {-# INLINABLE prettyListMC #-}

instance PrettyMC Int8 where
  prettyMC _ = pretty
  {-# INLINABLE prettyMC #-}
  prettyListMC _ = prettyList
  {-# INLINABLE prettyListMC #-}

instance PrettyMC Int16 where
  prettyMC _ = pretty
  {-# INLINABLE prettyMC #-}
  prettyListMC _ = prettyList
  {-# INLINABLE prettyListMC #-}

instance PrettyMC Int32 where
  prettyMC _ = pretty
  {-# INLINABLE prettyMC #-}
  prettyListMC _ = prettyList
  {-# INLINABLE prettyListMC #-}

instance PrettyMC Int64 where
  prettyMC _ = pretty
  {-# INLINABLE prettyMC #-}
  prettyListMC _ = prettyList
  {-# INLINABLE prettyListMC #-}

instance PrettyMC Integer where
  prettyMC _ = pretty
  {-# INLINABLE prettyMC #-}
  prettyListMC _ = prettyList
  {-# INLINABLE prettyListMC #-}

instance PrettyMC Natural where
  prettyMC _ = pretty
  {-# INLINABLE prettyMC #-}
  prettyListMC _ = prettyList
  {-# INLINABLE prettyListMC #-}

instance PrettyMC Word where
  prettyMC _ = pretty
  {-# INLINABLE prettyMC #-}
  prettyListMC _ = prettyList
  {-# INLINABLE prettyListMC #-}

instance PrettyMC Word8 where
  prettyMC _ = pretty
  {-# INLINABLE prettyMC #-}
  prettyListMC _ = prettyList
  {-# INLINABLE prettyListMC #-}

instance PrettyMC Word16 where
  prettyMC _ = pretty
  {-# INLINABLE prettyMC #-}
  prettyListMC _ = prettyList
  {-# INLINABLE prettyListMC #-}

instance PrettyMC Word32 where
  prettyMC _ = pretty
  {-# INLINABLE prettyMC #-}
  prettyListMC _ = prettyList
  {-# INLINABLE prettyListMC #-}

instance PrettyMC Word64 where
  prettyMC _ = pretty
  {-# INLINABLE prettyMC #-}
  prettyListMC _ = prettyList
  {-# INLINABLE prettyListMC #-}

instance PrettyMC () where
  prettyMC _ = pretty
  {-# INLINABLE prettyMC #-}
  prettyListMC _ = prettyList
  {-# INLINABLE prettyListMC #-}

instance PrettyMC Void where
  prettyMC _ = pretty
  {-# INLINABLE prettyMC #-}
  prettyListMC _ = prettyList
  {-# INLINABLE prettyListMC #-}

instance PrettyMC Text.Text where
  prettyMC _ = pretty
  {-# INLINABLE prettyMC #-}
  prettyListMC _ = prettyList
  {-# INLINABLE prettyListMC #-}

instance PrettyMC LazyText.Text where
  prettyMC _ = pretty
  {-# INLINABLE prettyMC #-}
  prettyListMC _ = prettyList
  {-# INLINABLE prettyListMC #-}

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
{-# INLINEABLE prettyIndent #-}
