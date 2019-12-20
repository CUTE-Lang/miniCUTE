{-# LANGUAGE GADTs #-}
-- |
-- Copyright: (c) 2018-present Junyoung Clare Jang
-- License: BSD 3-Clause
--
-- Parser functions for G-Machine
module Minicute.Parser.GMachine.Parser
  ( Parser

  , GMachineProgram
  , gMachineProgram
  ) where

import Data.Functor
import Minicute.Data.Common
import Minicute.Data.GMachine.Instruction
import Minicute.Parser.Common
import Minicute.Parser.Common.Parser
import Text.Megaparsec

import qualified Minicute.Parser.Common.Lexer as L

-- |
-- A parser for a G-Machine program.
gMachineProgram :: Parser GMachineProgram
gMachineProgram = many gMachineSupercombinator
{-# INLINE gMachineProgram #-}

gMachineSupercombinator :: Parser GMachineSupercombinator
gMachineSupercombinator
  = (,,)
    <$> (Identifier <$> L.identifier)
    <*> between (L.symbol "<") (L.symbol ">") L.integer
    <*> between (L.symbol "{") (L.symbol "}") gMachineExpression
{-# INLINABLE gMachineSupercombinator #-}

gMachineExpression :: Parser GMachineExpression
gMachineExpression = endBy instruction separator
{-# INLINE gMachineExpression #-}

-- |
-- __TODO: Add a more appropriate lexer instead of 'L.symbol'.__
-- 'L.symbol' does not check whether following character is a space or not.
instruction :: Parser Instruction
instruction
  = choice
    [ L.symbol "MakeInteger" >> IMakeInteger <$> L.integer
    , L.symbol "MakeConstructor" >> IMakeConstructor <$> L.integer <*> L.integer
    , L.symbol "MakeStructure" >> IMakeStructure <$> L.integer <*> L.integer
    , L.symbol "MakeApplication" $> IMakeApplication
    , L.symbol "MakeGlobal" >> IMakeGlobal <$> (Identifier <$> L.gMachineIdentifier)
    , L.symbol "MakePlaceholders" >> IMakePlaceholders <$> L.integer

    , L.symbol "Pop" >> IPop <$> L.integer
    , L.symbol "Dig" >> IDig <$> L.integer
    , try (L.symbol "Update" >> IUpdate <$> L.integer)
    , L.symbol "Copy" >> ICopy <$> L.integer

    , L.symbol "PushBasicValue" >> IPushBasicValue <$> L.integer
    , L.symbol "PushExtractedValue" $> IPushExtractedValue
    , L.symbol "WrapAsInteger" $> IWrapAsInteger
    , L.symbol "WrapAsStructure" $> IWrapAsStructure
    , L.symbol "UpdateAsInteger" >> IUpdateAsInteger <$> L.integer
    , L.symbol "UpdateAsStructure" >> IUpdateAsStructure <$> L.integer

    , L.symbol "Primitive" >> IPrimitive <$> primitive

    , L.symbol "Unwind" $> IUnwind
    , L.symbol "Destruct" >> IDestruct <$> L.integer

    , L.symbol "Eval" $> IEval
    , L.symbol "Return" $> IReturn

    , L.symbol "Match" >> IMatch <$> matchTable
    ]

matchTable :: Parser MatchTable
matchTable
  = MatchTable <$> between (L.symbol "{") (L.symbol "}") (many matchEntry)
{-# INLINABLE matchTable #-}

matchEntry :: Parser MatchEntry
matchEntry
  = (MatchEntry .)
    . (,)
    <$> L.integer <* L.symbol "->"
    <*> gMachineExpression
{-# INLINABLE matchEntry #-}


separator :: (MonadParser e s m) => m ()
separator = L.symbol ";"
{-# INLINABLE separator #-}
