{-# LANGUAGE TemplateHaskell #-}
module Test.Minicute.Utils where

import Data.Char
import Data.List
import Data.List.Extra
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Minicute.Data.String
import Minicute.Parser.Parser
import Text.Megaparsec

qqMini :: QuasiQuoter
qqMini
  = QuasiQuoter
    { quoteExp = qqMiniExp
    , quotePat = const . fail $ "qqCode cannot be used as a pattern"
    , quoteType = const . fail $ "qqCode cannot be used as a type"
    , quoteDec = const . fail $ "qqCode cannot be used as a declaration"
    }

qqCode :: QuasiQuoter
qqCode
  = QuasiQuoter
    { quoteExp = qqCodeExp
    , quotePat = const . fail $ "qqCode cannot be used as a pattern"
    , quoteType = const . fail $ "qqCode cannot be used as a type"
    , quoteDec = const . fail $ "qqCode cannot be used as a declaration"
    }

qqMiniExp :: String -> Q Exp
qqMiniExp = parseCaseExp . parseExp . qqCodeExp
  where
    parseExp :: Q Exp -> Q Exp
    parseExp e
      = [| parse mainProgramL "" $(e) |]
    parseCaseExp :: Q Exp -> Q Exp
    parseCaseExp e
      = [| case $(e) of
             Right result -> result
             Left err -> error (errorBundlePretty err)
        |]

qqCodeExp :: String -> Q Exp
qqCodeExp = litE . stringL . updateString . toUnix
  where
    updateString = dropEnd 1 . unlines . adjustIndent . trimEndEmptyLines . trimStartEmptyLines . lines

    trimStartEmptyLines = dropWhile isEmpty
    trimEndEmptyLines = dropWhileEnd isEmpty

    isEmpty = all isSpace

    adjustIndent ls = indentedLs
      where
        indentedLs = fmap (drop indent) ls
        indent = minimum . fmap (length . takeWhile isSpace) $ ls
