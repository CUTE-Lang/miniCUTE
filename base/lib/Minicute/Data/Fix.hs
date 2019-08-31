{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Definition for type-level fixpoints with various arguments.
module Minicute.Data.Fix
  ( Fix( .. )
  , Fix'

  , Fix2( .. )
  , Fix2'
  ) where

import Control.Lens.TH
import Data.Data
import Data.Function
import GHC.Generics
import GHC.Read ( lex )
import GHC.Show ( appPrec, appPrec1 )
import Language.Haskell.TH.Syntax

-- |
-- The type-level fixpoint with no extra arguments.
newtype Fix f = Fix { unFix :: Fix' f }
  deriving ( Generic
           , Typeable
           )
-- |
-- The internal type of 'Fix'.
type Fix' f = f (Fix f)

deriving instance (Typeable f, Data (f (Fix f))) => Data (Fix f)
deriving instance (Lift (f (Fix f))) => Lift (Fix f)

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

-- |
-- The type-level fixpoint with an extra argument.
newtype Fix2 f a = Fix2 { unFix2 :: Fix2' f a }
  deriving ( Generic
           , Typeable
           )
-- |
-- The internal type of 'Fix2'.
type Fix2' f a = f (Fix2 f) a

deriving instance (Typeable f, Typeable a, Data (f (Fix2 f) a)) => Data (Fix2 f a)
deriving instance (Lift (f (Fix2 f) a)) => Lift (Fix2 f a)

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


makeWrapped ''Fix

makeWrapped ''Fix2
