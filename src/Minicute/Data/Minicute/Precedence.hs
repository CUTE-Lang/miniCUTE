{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Types for precedences of primitive operators in miniCUTE
module Minicute.Data.Minicute.Precedence
  ( module Minicute.Data.Minicute.Common


  , Precedence( .. )

  , OperatorName
  , PrecedenceTable
  , PrecedenceTableEntry

  , isInfix

  , defaultPrecedenceTable
  , binaryPrecedenceTable
  , binaryIntegerPrecendenceTable
  , binaryDataPrecendenceTable

  , miniApplicationPrecedence
  , miniApplicationPrecedence1

  , prettyBinaryExpressionPrec
  ) where

import Data.Data
import GHC.Generics
import Language.Haskell.TH.Syntax
import Minicute.Data.Minicute.Common

import qualified Data.Text.Prettyprint.Doc as PP

-- |
-- Precedence of a unary/binary operator
data Precedence
  = PInfixN { precedence :: Int }
  | PInfixL { precedence :: Int }
  | PInfixR { precedence :: Int }
  | PPrefix { precedence :: Int }
  | PPostfix { precedence :: Int }
  deriving ( Generic
           , Typeable
           , Data
           , Lift
           , Eq
           , Ord
           , Show
           , Read
           )

-- |
-- Name of an operator
type OperatorName = String
-- |
-- Precedence character of an operator
type PrecedenceTableEntry = (OperatorName, Precedence)
-- |
-- Precedence characters of operators
type PrecedenceTable = [PrecedenceTableEntry]

-- |
-- Check whether the input operator is infix (i.e. binary).
isInfix :: Precedence -> Bool
isInfix (PInfixN _) = True
isInfix (PInfixL _) = True
isInfix (PInfixR _) = True
isInfix _ = False
{-# INLINEABLE isInfix #-}

-- |
-- All predefined precedences.
--
-- All precedences should be smaller than 'miniApplicationPrecedence'.
-- Where do I need to check this condition?
defaultPrecedenceTable :: PrecedenceTable
defaultPrecedenceTable
  = binaryPrecedenceTable

-- |
-- All predefined precedences of binary operators.
binaryPrecedenceTable :: PrecedenceTable
binaryPrecedenceTable
  = binaryDataPrecendenceTable
    <> binaryIntegerPrecendenceTable

-- |
-- All predefined precedences of binary integer operators.
binaryIntegerPrecendenceTable :: PrecedenceTable
binaryIntegerPrecendenceTable
  = [ ("+", PInfixL 40)
    , ("-", PInfixL 40)
    , ("*", PInfixL 50)
    , ("/", PInfixL 50)
    ]

-- |
-- All predefined precedences of binary data structure operators.
binaryDataPrecendenceTable :: PrecedenceTable
binaryDataPrecendenceTable
  = [ (">=", PInfixL 10)
    , (">", PInfixL 10)
    , ("<=", PInfixL 10)
    , ("<", PInfixL 10)
    , ("==", PInfixL 10)
    , ("!=", PInfixL 10)
    ]

-- |
-- The maximum precedence value in miniCUTE.
miniApplicationPrecedence :: Int
miniApplicationPrecedence = 100
{-# INLINEABLE miniApplicationPrecedence #-}

-- |
-- The supremum precedence value in miniCUTE.
miniApplicationPrecedence1 :: Int
miniApplicationPrecedence1 = 101
{-# INLINEABLE miniApplicationPrecedence1 #-}


-- |
-- @prettyBinaryExpressionPrec p op opPrec e1 e2@ make a document that includes
-- e1, op and e2 in the order, and pass appropriate precedences @p1@ and @p2@ to
-- @prettyPrec p1 e1@ and @prettyPrec p2 e2@ to give proper parenthesis.
prettyBinaryExpressionPrec :: Int -> Precedence -> PP.Doc ann -> (Int -> PP.Doc ann) -> (Int -> PP.Doc ann) -> PP.Doc ann
prettyBinaryExpressionPrec p opPrec opDoc e1DocF e2DocF
  = (if p > opP then PP.parens else id) . PP.hsep
    $ [ e1DocF leftP
      , opDoc
      , e2DocF rightP
      ]
  where
    (leftP, opP, rightP)
      = case opPrec of
          PInfixN opP' -> (opP' + 1, opP', opP' + 1)
          PInfixL opP' -> (opP', opP', opP' + 1)
          PInfixR opP' -> (opP' + 1, opP', opP')
          _ -> (miniApplicationPrecedence1, miniApplicationPrecedence, miniApplicationPrecedence1)
{-# INLINEABLE prettyBinaryExpressionPrec #-}
