module Minicute.Utils.TH
  ( qqMiniMainL
  , qqMiniMain
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
import Text.Megaparsec

qqMiniMainL :: QuasiQuoter
qqMiniMainL
  = QuasiQuoter
    { quoteExp = qqMiniMainLExp
    , quotePat = const . fail $ "qqMiniMainLExp cannot be used as a pattern"
    , quoteType = const . fail $ "qqMiniMainLExp cannot be used as a type"
    , quoteDec = const . fail $ "qqMiniMainLExp cannot be used as a declaration"
    }

qqMiniMain :: QuasiQuoter
qqMiniMain
  = QuasiQuoter
    { quoteExp = qqMiniMainExp
    , quotePat = const . fail $ "qqMiniMainExp cannot be used as a pattern"
    , quoteType = const . fail $ "qqMiniMainExp cannot be used as a type"
    , quoteDec = const . fail $ "qqMiniMainExp cannot be used as a declaration"
    }

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
