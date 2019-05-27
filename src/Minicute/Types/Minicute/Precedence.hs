{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
module Minicute.Types.Minicute.Precedence where

import Data.Data
import GHC.Generics
import Language.Haskell.TH.Syntax

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

type PrecedenceTableEntry = (String, Precedence)
type PrecedenceTable = [PrecedenceTableEntry]

isInfix :: Precedence -> Bool
isInfix (PInfixN _) = True
isInfix (PInfixL _) = True
isInfix (PInfixR _) = True
isInfix _ = False
{-# INLINEABLE isInfix #-}

-- |
-- All precedences should be smaller than 'applicationPrecedence'.
-- Where do I need to check this condition?
defaultPrecedenceTable :: PrecedenceTable
defaultPrecedenceTable
  = [ (">=", PInfixL 10)
    , (">", PInfixL 10)
    , ("<=", PInfixL 10)
    , ("<", PInfixL 10)
    , ("==", PInfixL 10)
    , ("!=", PInfixL 10)
    , ("+", PInfixL 40)
    , ("-", PInfixL 40)
    , ("*", PInfixL 50)
    , ("/", PInfixL 50)
    ]

applicationPrecedence :: Int
applicationPrecedence = 100
{-# INLINEABLE applicationPrecedence #-}

applicationPrecedence1 :: Int
applicationPrecedence1 = 101
{-# INLINEABLE applicationPrecedence1 #-}
