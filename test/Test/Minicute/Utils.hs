{-# LANGUAGE TemplateHaskell #-}
module Test.Minicute.Utils where

import Data.Char
import Data.List
import Data.List.Extra
import Data.String.Minicute
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Minicute.Parser.Parser
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
qqMiniMainLExp = parseCaseExp . parseExp . qqRawCodeExp
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

qqMiniMainExp :: String -> Q Exp
qqMiniMainExp = parseCaseExp . parseExp . qqRawCodeExp
  where
    parseExp :: Q Exp -> Q Exp
    parseExp e
      = [| parse mainProgram "" $(e) |]
    parseCaseExp :: Q Exp -> Q Exp
    parseCaseExp e
      = [| case $(e) of
             Right result -> result
             Left err -> error (errorBundlePretty err)
        |]

qqRawCodeExp :: String -> Q Exp
qqRawCodeExp = litE . stringL . updateString . toUnix
  where
    updateString = dropEnd 1 . unlines . adjustIndent . trimEndEmptyLines . trimStartEmptyLines . lines

    trimStartEmptyLines = dropWhile isEmpty
    trimEndEmptyLines = dropWhileEnd isEmpty

    isEmpty = all isSpace

    adjustIndent ls = indentedLs
      where
        indentedLs = fmap (drop indent) ls
        indent = minimum . fmap (length . takeWhile isSpace) $ ls
