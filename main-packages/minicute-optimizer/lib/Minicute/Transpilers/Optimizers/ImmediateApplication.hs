{-# LANGUAGE DataKinds #-}
-- |
-- Copyright: (c) 2018-present Junyoung Clare Jang
-- License: BSD 3-Clause
--
-- Optimizers to remove immediate applications.
module Minicute.Transpilers.Optimizers.ImmediateApplication
  ( module Minicute.Data.Minicute.Program

  , immediateApplicationMC
  ) where

import Control.Lens.Each
import Control.Lens.Operators
import Control.Lens.Plated ( transformOf )
import Control.Lens.Wrapped ( _Wrapped )
import Data.Data ( Data )
import Data.Data.Lens ( uniplate )
import Minicute.Data.Minicute.Program

-- |
-- An optimizer to remove immediate applications in a whole program.
immediateApplicationMC :: (Data a) => Program 'Simple 'MC a -> Program 'Simple 'MC a
immediateApplicationMC
  = _Wrapped . each . _supercombinatorBody %~ immediateApplicationEMC
{-# INLINE immediateApplicationMC #-}

-- |
-- An optimizer to remove immediate applications in an expression.
immediateApplicationEMC :: (Data a) => Expression 'Simple 'MC a -> Expression 'Simple 'MC a
immediateApplicationEMC = transformOf uniplate go
  where
    go :: Expression 'Simple 'MC a -> Expression 'Simple 'MC a
    go (EApplication _ (ELambda _ (v : args') expr) e2)
      = case args' of
          _ : _ -> ELambda () args' expr'
          _ -> expr'
      where
        expr' = ELet () NonRecursive [LetDefinition (v, e2)] expr
    go e = e
{-# INLINABLE immediateApplicationEMC #-}
