{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Minicute.Parser.GMachine.Parser
  ( Parser

  , GMachineProgram
  , gMachineProgram
  ) where

import Data.Functor
import Minicute.Parser.Types
import Minicute.Types.GMachine.Instruction
import Text.Megaparsec

import qualified Control.Monad.Combinators as Comb
import qualified Minicute.Parser.Lexer as L

gMachineProgram :: Parser GMachineProgram
gMachineProgram = Comb.many gMachineSupercombinator

gMachineSupercombinator :: Parser GMachineSupercombinator
gMachineSupercombinator
  = (,,)
    <$> L.identifier
    <*> Comb.between (L.symbol "<") (L.symbol ">") L.integer
    <*> Comb.between (L.symbol "{") (L.symbol "}") gMachineExpression

gMachineExpression :: Parser GMachineExpression
gMachineExpression = Comb.endBy instruction separator

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

    , L.symbol "Eval" $> IEval
    , L.symbol "Return" $> IReturn
    ]

primitiveOperator :: Parser PrimitiveOperator
primitiveOperator
  = choice
    [ L.symbol "+" $> POAdd
    , L.symbol "-" $> POSub
    , L.symbol "*" $> POMul
    , L.symbol "/" $> PODiv
    ]


separator :: (MonadParser e s m) => m ()
separator = L.symbol ";"
{-# INLINEABLE separator #-}
