{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module Minicute.Data.Fix
  ( Fix( .. )
  , Fix2( .. )
  ) where

import Data.Data
import Data.Function
import GHC.Generics
import GHC.Read ( lex )
import GHC.Show ( appPrec, appPrec1 )

newtype Fix f = Fix { unFix :: f (Fix f) }
  deriving ( Generic
           , Typeable
           )

deriving instance (Typeable f, Data (f (Fix f))) => Data (Fix f)

instance (Eq (f (Fix f))) => Eq (Fix f) where
  (==) = (==) `on` unFix

instance (Ord (f (Fix f))) => Ord (Fix f) where
  compare = compare `on` unFix

instance (Show (f (Fix f))) => Show (Fix f) where
  showsPrec n x
    = showParen (n > appPrec)
      $ ("Fix " <>) . showsPrec appPrec1 (unFix x)

instance (Read (f (Fix f))) => Read (Fix f) where
  readsPrec d
    = readParen (d > appPrec)
      $ \s ->
          [ (Fix m, s'')
          | ("Fix", s') <- lex s
          , (m, s'') <- readsPrec appPrec1 s'
          ]

newtype Fix2 f a = Fix2 { unFix2 :: f (Fix2 f) a }
  deriving ( Generic
           , Typeable
           )

deriving instance (Typeable f, Typeable a, Data (f (Fix2 f) a)) => Data (Fix2 f a)

instance (Eq (f (Fix2 f) a)) => Eq (Fix2 f a) where
  (==) = (==) `on` unFix2

instance (Ord (f (Fix2 f) a)) => Ord (Fix2 f a) where
  compare = compare `on` unFix2

instance (Show (f (Fix2 f) a)) => Show (Fix2 f a) where
  showsPrec n x
    = showParen (n > appPrec)
      $ ("Fix2 " <>) . showsPrec appPrec1 (unFix2 x)

instance (Read (f (Fix2 f) a)) => Read (Fix2 f a) where
  readsPrec d
    = readParen (d > appPrec)
      $ \s ->
          [ (Fix2 m, s'')
          | ("Fix2", s') <- lex s
          , (m, s'') <- readsPrec appPrec1 s'
          ]
