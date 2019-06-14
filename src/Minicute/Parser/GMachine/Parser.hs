{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Parser functions for G-Machine
module Minicute.Parser.GMachine.Parser
  ( Parser

  , GMachineProgram
  , gMachineProgram
  ) where

import Data.Functor
import Minicute.Parser.Types
import Minicute.Types.GMachine.Instruction
import Text.Megaparsec

import qualified Minicute.Parser.Lexer as L

-- |
-- A parser for a G-Machine program.
gMachineProgram :: Parser GMachineProgram
gMachineProgram = many gMachineSupercombinator

gMachineSupercombinator :: Parser GMachineSupercombinator
gMachineSupercombinator
  = (,,)
    <$> L.identifier
    <*> between (L.symbol "<") (L.symbol ">") L.integer
    <*> between (L.symbol "{") (L.symbol "}") gMachineExpression

gMachineExpression :: Parser GMachineExpression
gMachineExpression = endBy instruction separator

-- |
-- __TODO: Add a more appropriate lexer instead of 'L.symbol'.__
-- 'L.symbol' does not check whether following character is a space or not.
instruction :: Parser Instruction
instruction
  = choice
    [ L.symbol "MakeInteger" >> IMakeInteger <$> L.integer
    , L.symbol "MakeConstructor" >> IMakeConstructor <$> L.integer <*> L.integer
    , L.symbol "MakeApplication" $> IMakeApplication
    , L.symbol "MakeGlobal" >> IMakeGlobal <$> L.gMachineIdentifier
    , L.symbol "MakePlaceholders" >> IMakePlaceholders <$> L.integer

    , L.symbol "Pop" >> IPop <$> L.integer
    , L.symbol "Dig" >> IDig <$> L.integer
    , try (L.symbol "Update" >> IUpdate <$> L.integer)
    , L.symbol "CopyArgument" >> ICopyArgument <$> L.integer

    , L.symbol "PushBasicValue" >> IPushBasicValue <$> L.integer
    , L.symbol "PushExtractedValue" $> IPushExtractedValue
    , L.symbol "WrapAsInteger" $> IWrapAsInteger
    , L.symbol "WrapAsConstructor" $> IWrapAsConstructor
    , L.symbol "UpdateAsInteger" >> IUpdateAsInteger <$> L.integer
    , L.symbol "UpdateAsConstructor" >> IUpdateAsConstructor <$> L.integer

    , L.symbol "Primitive" >> IPrimitive <$> primitiveOperator

    , L.symbol "Unwind" $> IUnwind
    , L.symbol "Destruct" >> IDestruct <$> L.integer

    , L.symbol "Eval" $> IEval
    , L.symbol "Return" $> IReturn

    , L.symbol "Match" >> IMatch <$> matchTable
    ]

primitiveOperator :: Parser PrimitiveOperator
primitiveOperator
  = choice
    [ L.symbol "+" $> POAdd
    , L.symbol "-" $> POSub
    , L.symbol "*" $> POMul
    , L.symbol "/" $> PODiv
    ]

matchTable :: Parser MatchTable
matchTable
  = MatchTable <$> between (L.symbol "{") (L.symbol "}") (many matchEntry)

matchEntry :: Parser MatchEntry
matchEntry
  = MatchEntry
    <$> ( (,)
          <$> L.integer <* L.symbol "->"
          <*> gMachineExpression
        )


separator :: (MonadParser e s m) => m ()
separator = L.symbol ";"
{-# INLINEABLE separator #-}
