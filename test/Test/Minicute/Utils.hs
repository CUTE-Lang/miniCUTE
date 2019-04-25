module Test.Minicute.Utils where

import Data.Char
import Data.List
import Data.List.Extra
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Minicute.Data.String

qqMini :: QuasiQuoter
qqMini
  = QuasiQuoter
    { quoteExp = qqMiniExp
    , quotePat = const . fail $ "qqMini cannot be used as a pattern"
    , quoteType = const . fail $ "qqMini cannot be used as a type"
    , quoteDec = const . fail $ "qqMini cannot be used as a declaration"
    }

qqMiniExp :: String -> Q Exp
qqMiniExp = return . LitE . StringL . updateString . toUnix
  where
    updateString = dropEnd 1 . unlines . adjustIndent . trimEndEmptyLines . trimStartEmptyLines . lines

    trimStartEmptyLines = dropWhile isEmpty
    trimEndEmptyLines = dropWhileEnd isEmpty

    isEmpty = all isSpace

    adjustIndent ls = indentedLs
      where
        indentedLs = fmap (drop indent) ls
        indent = minimum . fmap (length . takeWhile isSpace) $ ls
