module Minicute.Parser.Precedence where

data Precedence
  = PInfixN { precedence :: Int }
  | PInfixL { precedence :: Int }
  | PInfixR { precedence :: Int }
  | PPrefix { precedence :: Int }
  | PPostfix { precedence :: Int }
  deriving ( Eq
           , Show
           )

type PrecedenceTableEntry = (String, Precedence)
type PrecedenceTable = [PrecedenceTableEntry]

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
