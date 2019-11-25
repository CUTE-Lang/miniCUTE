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
import Minicute.Data.Common
import Minicute.Parser.Common
import Text.Megaparsec

import qualified Control.Monad.Combinators.Expr as CombExpr
import qualified Minicute.Parser.Common.Lexer as L

primitive :: Parser Primitive
primitive
  = choice
    (makePrimParser . fst <$> primitivePrecedenceTable)
  where
    makePrimParser :: (MonadParser e s m) => Primitive -> m Primitive
    makePrimParser prim = L.symbol (toString prim) $> prim
{-# INLINEABLE primitive #-}


createPrimitiveOperatorTable :: (MonadParser e s m) => (Primitive -> expr) -> (expr -> expr -> expr) -> (expr -> expr -> expr -> expr) -> PrecedenceTable Primitive -> [[CombExpr.Operator m expr]]
createPrimitiveOperatorTable cPrim cUn cBar = (fmap . fmap) (createPrimitiveOperator cPrim cUn cBar) . groupSortOn (negate . precedence . snd)
{-# INLINEABLE createPrimitiveOperatorTable #-}

createPrimitiveOperator :: (MonadParser e s m) => (Primitive -> expr) -> (expr -> expr -> expr) -> (expr -> expr -> expr -> expr) -> PrecedenceTableEntry Primitive -> CombExpr.Operator m expr
createPrimitiveOperator cPrim cUn cBin (prim, prec)
  = case prec of
      PInfixN _ ->
        CombExpr.InfixN (createBinaryPrimitiveFunctionParser cPrim cBin prim)
      PInfixL _ ->
        CombExpr.InfixL (createBinaryPrimitiveFunctionParser cPrim cBin prim)
      PInfixR _ ->
        CombExpr.InfixR (createBinaryPrimitiveFunctionParser cPrim cBin prim)
      PPrefix _ ->
        CombExpr.Prefix (createUnaryPrimitiveFunctionParser cPrim cUn prim)
      PPostfix _ ->
        CombExpr.Postfix (createUnaryPrimitiveFunctionParser cPrim cUn prim)

createBinaryPrimitiveFunctionParser :: (MonadParser e s m) => (Primitive -> expr) -> (expr -> expr -> expr -> expr) -> Primitive -> m (expr -> expr -> expr)
createBinaryPrimitiveFunctionParser cPrim cBin prim = (L.symbol primName <?> "binary primitive") $> cBin (cPrim prim)
  where
    primName = toString prim
{-# INLINEABLE createBinaryPrimitiveFunctionParser #-}

createUnaryPrimitiveFunctionParser :: (MonadParser e s m) => (Primitive -> expr) -> (expr -> expr -> expr) -> Primitive -> m (expr -> expr)
createUnaryPrimitiveFunctionParser cPrim cUn prim = (L.symbol primName <?> "unary primitive") $> cUn (cPrim prim)
  where
    primName = toString prim
{-# INLINEABLE createUnaryPrimitiveFunctionParser #-}
