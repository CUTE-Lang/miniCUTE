-- |
-- Utilities for miniCUTE compiler using TemplateHaskell
module Minicute.Utils.TH
  ( qqMiniMainMC
  , qqMiniMainLLMC

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
-- parse quoted string as a 'MainProgramMC'.
qqMiniMainMC :: QuasiQuoter
qqMiniMainMC
  = QuasiQuoter
    { quoteExp = qqMiniMainExpMC
    , quotePat = const . fail $ "qqMiniMainExpMC cannot be used as a pattern"
    , quoteType = const . fail $ "qqMiniMainExpMC cannot be used as a type"
    , quoteDec = const . fail $ "qqMiniMainExpMC cannot be used as a declaration"
    }

-- |
-- parse quoted string as a 'MainProgramLLMC'.
qqMiniMainLLMC :: QuasiQuoter
qqMiniMainLLMC
  = QuasiQuoter
    { quoteExp = qqMiniMainExpLLMC
    , quotePat = const . fail $ "qqMiniMainExpLLMC cannot be used as a pattern"
    , quoteType = const . fail $ "qqMiniMainExpLLMC cannot be used as a type"
    , quoteDec = const . fail $ "qqMiniMainExpLLMC cannot be used as a declaration"
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


qqMiniMainExpMC :: String -> Q Exp
qqMiniMainExpMC = lift . either (error . errorBundlePretty) id . parse mainProgramMC "" . normalizeCode

qqMiniMainExpLLMC :: String -> Q Exp
qqMiniMainExpLLMC = lift . either (error . errorBundlePretty) id . parse mainProgramLLMC "" . normalizeCode


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
