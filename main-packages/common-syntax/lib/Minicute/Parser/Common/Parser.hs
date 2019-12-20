{-# LANGUAGE GADTs #-}
-- |
-- Copyright: (c) 2018-present Junyoung Clare Jang
-- License: BSD 3-Clause
module Minicute.Parser.Common.Parser
  ( primitive

  , createPrimitiveOperatorTable
  ) where

import Data.Functor
import Data.List.Extra
import Data.Ord
import Minicute.Data.Common
import Minicute.Parser.Common
import Text.Megaparsec

import qualified Control.Monad.Combinators.Expr as CombExpr
import qualified Minicute.Parser.Common.Lexer as L

primitive :: Parser Primitive
primitive
  = choice
    $ makePrimParser . fst <$> primitivePrecedenceTable
  where
    makePrimParser :: Primitive -> Parser Primitive
    makePrimParser prim = L.symbol (toString prim) $> prim
{-# INLINABLE primitive #-}


createPrimitiveOperatorTable :: (MonadParser e s m) => (Primitive -> expr) -> (expr -> expr -> expr) -> (expr -> expr -> expr -> expr) -> PrecedenceTable Primitive -> [[CombExpr.Operator m expr]]
createPrimitiveOperatorTable cPrim cUn cBar
  = fmap (createPrimitiveOperator cPrim cUn cBar <$>)
    . groupSortOn (Down . precedence . snd)
{-# INLINABLE createPrimitiveOperatorTable #-}

createPrimitiveOperator :: (MonadParser e s m) => (Primitive -> expr) -> (expr -> expr -> expr) -> (expr -> expr -> expr -> expr) -> PrecedenceTableEntry Primitive -> CombExpr.Operator m expr
createPrimitiveOperator cPrim cUn cBin (prim, prec)
  = case prec of
      PInfixN _ -> CombExpr.InfixN bin
      PInfixL _ -> CombExpr.InfixL bin
      PInfixR _ -> CombExpr.InfixR bin
      PPrefix _ -> CombExpr.Prefix un
      PPostfix _ -> CombExpr.Postfix un
  where
    bin = createBinaryPrimitiveFunctionParser cPrim cBin prim
    un = createUnaryPrimitiveFunctionParser cPrim cUn prim

    {-# INLINE bin #-}
    {-# INLINE un #-}
{-# INLINABLE createPrimitiveOperator #-}

createBinaryPrimitiveFunctionParser :: (MonadParser e s m) => (Primitive -> expr) -> (expr -> expr -> expr -> expr) -> Primitive -> m (expr -> expr -> expr)
createBinaryPrimitiveFunctionParser cPrim cBin prim
  = L.symbol primName $> cBin (cPrim prim)
    <?> "binary primitive"
  where
    primName = toString prim

    {-# INLINE primName #-}
{-# INLINABLE createBinaryPrimitiveFunctionParser #-}

createUnaryPrimitiveFunctionParser :: (MonadParser e s m) => (Primitive -> expr) -> (expr -> expr -> expr) -> Primitive -> m (expr -> expr)
createUnaryPrimitiveFunctionParser cPrim cUn prim
  = L.symbol primName $> cUn (cPrim prim)
    <?> "unary primitive"
  where
    primName = toString prim

    {-# INLINE primName #-}
{-# INLINABLE createUnaryPrimitiveFunctionParser #-}
