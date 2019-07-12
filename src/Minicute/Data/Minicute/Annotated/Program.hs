-- |
-- Types for annotated programs
module Minicute.Data.Minicute.Annotated.Program
  ( module Minicute.Data.Minicute.Annotated.Expression
  , module Minicute.Data.Minicute.Program


  , AnnotatedSupercombinatorMC
  , MainAnnotatedSupercombinatorMC


  , AnnotatedProgramMC
  , MainAnnotatedProgramMC
  ) where

import Minicute.Data.Minicute.Annotated.Expression
import Minicute.Data.Minicute.Program


-- |
-- @AnnotatedSupercombinatorMC ann@ is a @SupercombinatorMC@ annotated by @ann@.
type AnnotatedSupercombinatorMC ann = Supercombinator (AnnotatedExpressionMC ann)
-- |
-- @MainAnnotatedSupercombinatorMC ann@ is a @MainSupercombinatorMC@ annotated by @ann@.
type MainAnnotatedSupercombinatorMC ann = Supercombinator (AnnotatedExpressionMC ann) Identifier


-- |
-- @AnnotatedProgramMC ann a@ is a @ProgramMC@ annotated by @ann@.
type AnnotatedProgramMC ann = Program (AnnotatedExpressionMC ann)
-- |
-- @MainAnnotatedProgramMC ann a@ is a @MainProgramMC@ annotated by @ann@.
type MainAnnotatedProgramMC ann = Program (AnnotatedExpressionMC ann) Identifier
