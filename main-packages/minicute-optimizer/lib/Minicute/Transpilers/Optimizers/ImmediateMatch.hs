-- |
-- Copyright: (c) 2018-present Junyoung Clare Jang
-- License: BSD 3-Clause
--
-- Optimizers to remove immediate matches.
module Minicute.Transpilers.Optimizers.ImmediateMatch
  ( immediateMatchMainMC
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
immediateMatchMainMC :: MainProgramMC -> MainProgramMC
immediateMatchMainMC = _Wrapped . each . _supercombinatorBody %~ immediateMatchMainEMC

-- |
-- An optimizer to remove immediate matches in an expression.
immediateMatchMainEMC :: MainExpressionMC -> MainExpressionMC
immediateMatchMainEMC = transformOf uniplate go
  where
    go (ELet flag lDefs (EMatch (EVariable v) mCases))
      | Just vLDef <- lookupLDefsL v lDefs
      , Just (tag, argExprs) <- destructStructureExpression (vLDef ^. _letDefinitionBody)
      , Just vMCase <- lookupMCasesL tag mCases
      = let
          argBinders = vMCase ^. _matchCaseArguments
          matchLDefs = zipWith (curry LetDefinition) argBinders argExprs

          innerExpr = vMCase ^. _matchCaseBody
          expr
            = if not (null matchLDefs)
              then immediateMatchMainEMC (ELet NonRecursive matchLDefs innerExpr)
              else innerExpr
        in
          ELet flag lDefs expr
    go e = e
