{-# LANGUAGE MagicHash #-}
module Minicute.Transpiler.VariablesRenaming
  ( renameVariablesMainL
  ) where

import Minicute.Types.Minicute.Program

type Renamer a = a -> a

renameVariablesMainL :: Renamer MainProgramL
renameVariablesMainL program = program

renameVariablesL :: Renamer a -> Renamer (ProgramL a)
renameVariablesL = undefined

renameVariables# :: Renamer a -> Renamer expr -> Renamer (Program# a expr)
renameVariables# = undefined

renameIdentifier :: Renamer Identifier
renameIdentifier = undefined
