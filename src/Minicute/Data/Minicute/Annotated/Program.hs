-- |
-- Types for annotated programs
module Minicute.Data.Minicute.Annotated.Program
  ( module Minicute.Data.Minicute.Annotated.Expression
  , module Minicute.Data.Minicute.Program


  , MainAnnotatedSupercombinatorL


  , MainAnnotatedProgramL
  ) where

import Minicute.Data.Minicute.Annotated.Expression
import Minicute.Data.Minicute.Program


-- |
-- @MainAnnotatedSupercombinatorL ann@ is a @MainSupercombinatorL@ annotated by @ann@.
type MainAnnotatedSupercombinatorL ann = Supercombinator (AnnotatedExpressionL ann) Identifier


-- |
-- @MainAnnotatedProgram ann a@ is a @MainProgram@ annotated by @ann@.
type MainAnnotatedProgramL ann = Program (AnnotatedExpressionL ann) Identifier
