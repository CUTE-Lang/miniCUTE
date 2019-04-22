{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module Minicute.Data.Fix
  ( Fix( .. )
  , Fix2( .. )
  ) where

import Data.Data
import Data.Function
import GHC.Show ( appPrec, appPrec1 )

newtype Fix f = Fix { unFix :: f (Fix f) }

deriving instance (Typeable f, Data (f (Fix f))) => Data (Fix f)

instance (Eq (f (Fix f))) => Eq (Fix f) where
  (==) = (==) `on` unFix

instance (Show (f (Fix f))) => Show (Fix f) where
  showsPrec n x
    = showParen (n > appPrec)
    $ ("Fix " <>) . showsPrec appPrec1 (unFix x)

newtype Fix2 f a = Fix2 { unFix2 :: f (Fix2 f) a }

deriving instance (Typeable f, Typeable a, Data (f (Fix2 f) a)) => Data (Fix2 f a)

instance (Eq (f (Fix2 f) a)) => Eq (Fix2 f a) where
  (==) = (==) `on` unFix2

instance (Show (f (Fix2 f) a)) => Show (Fix2 f a) where
  showsPrec n x
    = showParen (n > appPrec)
    $ ("Fix2 " <>) . showsPrec appPrec1 (unFix2 x)
