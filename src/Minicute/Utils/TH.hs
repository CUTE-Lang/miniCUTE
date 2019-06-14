-- |
-- Utilities for miniCUTE compiler using TemplateHaskell
module Minicute.Utils.TH
  ( qqMiniMainL
  , qqMiniMain

  , qqGMachine

  , qqRawCode
  ) where

import Data.Char
import Data.List
import Data.List.Extra
import Data.String.Minicute
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Minicute.Parser.Minicute.Parser
import Minicute.Parser.GMachine.Parser
import Text.Megaparsec

-- |
-- parse quoted string as a 'MainProgramL'.
qqMiniMainL :: QuasiQuoter
qqMiniMainL
  = QuasiQuoter
    { quoteExp = qqMiniMainLExp
    , quotePat = const . fail $ "qqMiniMainLExp cannot be used as a pattern"
    , quoteType = const . fail $ "qqMiniMainLExp cannot be used as a type"
    , quoteDec = const . fail $ "qqMiniMainLExp cannot be used as a declaration"
    }

-- |
-- parse quoted string as a 'MainProgram'.
qqMiniMain :: QuasiQuoter
qqMiniMain
  = QuasiQuoter
    { quoteExp = qqMiniMainExp
    , quotePat = const . fail $ "qqMiniMainExp cannot be used as a pattern"
    , quoteType = const . fail $ "qqMiniMainExp cannot be used as a type"
    , quoteDec = const . fail $ "qqMiniMainExp cannot be used as a declaration"
    }


-- |
-- parse quoted string as a 'GMachineProgram'.
qqGMachine :: QuasiQuoter
qqGMachine
  = QuasiQuoter
    { quoteExp = qqGMachineExp
    , quotePat = const . fail $ "qqMiniMainExp cannot be used as a pattern"
    , quoteType = const . fail $ "qqMiniMainExp cannot be used as a type"
    , quoteDec = const . fail $ "qqMiniMainExp cannot be used as a declaration"
    }


-- |
-- parse quoted string as an indented code text.
qqRawCode :: QuasiQuoter
qqRawCode
  = QuasiQuoter
    { quoteExp = qqRawCodeExp
    , quotePat = const . fail $ "qqRawCode cannot be used as a pattern"
    , quoteType = const . fail $ "qqRawCode cannot be used as a type"
    , quoteDec = const . fail $ "qqRawCode cannot be used as a declaration"
    }


qqMiniMainLExp :: String -> Q Exp
qqMiniMainLExp = lift . either (error . errorBundlePretty) id . parse mainProgramL "" . normalizeCode

qqMiniMainExp :: String -> Q Exp
qqMiniMainExp = lift . either (error . errorBundlePretty) id . parse mainProgram "" . normalizeCode


qqGMachineExp :: String -> Q Exp
qqGMachineExp = lift . either (error . errorBundlePretty) id . parse gMachineProgram "" . normalizeCode


qqRawCodeExp :: String -> Q Exp
qqRawCodeExp = lift . normalizeCode

normalizeCode :: String -> String
normalizeCode = updateString . toUnix
  where
    updateString = dropEnd 1 . unlines . adjustIndent . trimEndEmptyLines . trimStartEmptyLines . lines

    trimStartEmptyLines = dropWhile isEmpty
    trimEndEmptyLines = dropWhileEnd isEmpty

    isEmpty = all isSpace

    adjustIndent ls = indentedLs
      where
        indentedLs = fmap (drop indent) ls
        indent = minimum . fmap (length . takeWhile isSpace) $ ls
