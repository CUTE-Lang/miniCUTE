{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Copyright: (c) 2018-present Junyoung Clare Jang
-- License: BSD 3-Clause
--
-- Types for miniCUTE programs
module Minicute.Data.Minicute.Program
  ( module Minicute.Data.Minicute.Expression

  , Supercombinator( .. )
  , MainSupercombinator

  , _supercombinatorBinder
  , _supercombinatorArguments
  , _supercombinatorBody


  , Program( .. )
  , MainProgram
  ) where

import Control.Lens.TH
import Control.Lens.Tuple
import Control.Lens.Type
import Control.Lens.Wrapped
import Data.Data ( Data, Typeable )
import Data.Text.Prettyprint.Doc.Minicute
import GHC.Generics ( Generic )
import Language.Haskell.TH.Syntax
import Minicute.Data.Minicute.Expression

import qualified Data.Text.Prettyprint.Doc as PP

-- |
-- A type for a supercombinator (top-level function definition)
--
-- [@Identifier@] the top-level identifier of the definition.
--
-- [@[a\]@] the arguments of the definition.
--
-- [@expr a@] the body expression of the definition.
newtype Supercombinator t l a
  = Supercombinator (Identifier, [a], Expression t l a)
  deriving ( Generic
           , Typeable
           )

deriving instance (Data a, Typeable t, Typeable l, Data (Expression t l a)) => Data (Supercombinator t l a)
deriving instance (Lift a, Lift (Expression t l a)) => Lift (Supercombinator t l a)
deriving instance (Eq a, Eq (Expression t l a)) => Eq (Supercombinator t l a)
deriving instance (Ord a, Ord (Expression t l a)) => Ord (Supercombinator t l a)
deriving instance (Show a, Show (Expression t l a)) => Show (Supercombinator t l a)

instance (PrettyMC a, PrettyMC (Expression t l a)) => PrettyMC (Supercombinator t l a) where
  prettyMC _ (Supercombinator (scId, argBinders, expr))
    = PP.hsep
      $ [prettyMC0 scId]
      <> (prettyMC0 <$> argBinders)
      <> [PP.equals, prettyMC0 expr]

-- |
-- A supercombinator of 'MainExpression'
type MainSupercombinator t l = Supercombinator t l Identifier


-- |
-- A type for a miniCUTE program
newtype Program t l a
  = Program [Supercombinator t l a]
  deriving ( Generic
           , Typeable
           )

deriving instance (Data a, Typeable t, Typeable l, Data (Expression t l a)) => Data (Program t l a)
deriving instance (Lift a, Lift (Expression t l a)) => Lift (Program t l a)
deriving instance (Eq a, Eq (Expression t l a)) => Eq (Program t l a)
deriving instance (Ord a, Ord (Expression t l a)) => Ord (Program t l a)
deriving instance (Show a, Show (Expression t l a)) => Show (Program t l a)

instance (PrettyMC a, PrettyMC (Expression t l a)) => PrettyMC (Program t l a) where
  prettyMC _ (Program scs)
    = PP.vcat . PP.punctuate PP.semi $ prettyMC0 <$> scs

-- |
-- A program of 'MainExpression'
type MainProgram t l = Program t l Identifier


makeWrapped ''Supercombinator

-- |
-- 'Lens' to extract the binder of 'Supercombinator'
_supercombinatorBinder :: Lens' (Supercombinator t l a) Identifier
_supercombinatorBinder = _Wrapped . _1
{-# INLINABLE _supercombinatorBinder #-}

-- |
-- 'Lens' to extract the list of arguments of 'Supercombinator'
_supercombinatorArguments :: Lens' (Supercombinator t l a) [a]
_supercombinatorArguments = _Wrapped . _2
{-# INLINABLE _supercombinatorArguments #-}

-- |
-- 'Lens' to extract the body expression of 'Supercombinator'
_supercombinatorBody :: Lens (Supercombinator t1 l1 a) (Supercombinator t2 l2 a) (Expression t1 l1 a) (Expression t2 l2 a)
_supercombinatorBody = _Wrapped . _3
{-# INLINABLE _supercombinatorBody #-}


makeWrapped ''Program
