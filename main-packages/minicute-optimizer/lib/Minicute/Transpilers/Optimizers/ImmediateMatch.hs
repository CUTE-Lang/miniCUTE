{-# LANGUAGE DataKinds #-}
-- |
-- Copyright: (c) 2018-present Junyoung Clare Jang
-- License: BSD 3-Clause
--
-- Optimizers to remove immediate matches.
module Minicute.Transpilers.Optimizers.ImmediateMatch
  ( module Minicute.Data.Minicute.Program

  , immediateMatchMainMC
  ) where

import Control.Lens.Each
import Control.Lens.Operators
import Control.Lens.Plated ( transformOf )
import Control.Lens.Wrapped ( _Wrapped )
import Data.Data.Lens ( uniplate )
import Minicute.Data.Minicute.Program
import Minicute.Utils.Minicute.Expression

-- |
-- An optimizer to remove immediate matches in a whole program.
immediateMatchMainMC :: MainProgram 'Simple 'MC -> MainProgram 'Simple 'MC
immediateMatchMainMC
  = _Wrapped . each . _supercombinatorBody %~ immediateMatchMainEMC
{-# INLINABLE immediateMatchMainMC #-}

-- |
-- An optimizer to remove immediate matches in an expression.
immediateMatchMainEMC :: MainExpression 'Simple 'MC -> MainExpression 'Simple 'MC
immediateMatchMainEMC = transformOf uniplate go
  where
    go (ELet _ flag lDefs (EMatch _ (EVariable _ v) mCases))
      | Just vLDef <- lookupLDefsL v lDefs
      , Just (tag, argExprs) <- destructStructureExpression (vLDef ^. _letDefinitionBody)
      , Just vMCase <- lookupMCasesL tag mCases
      = let
          argBinders = vMCase ^. _matchCaseArguments
          matchLDefs = zipWith (curry LetDefinition) argBinders argExprs

          innerExpr = vMCase ^. _matchCaseBody
          boundExpr = ELet () NonRecursive matchLDefs innerExpr
          expr
            = case matchLDefs of
                _ : _ ->
                  immediateMatchMainEMC boundExpr
                _ -> innerExpr
        in
          ELet () flag lDefs expr
    go e = e
{-# INLINABLE immediateMatchMainEMC #-}
