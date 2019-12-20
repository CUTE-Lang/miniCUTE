-- |
-- Copyright: (c) 2018-present Junyoung Clare Jang
-- License: BSD 3-Clause
--
-- Common utilities for miniCUTE compiler using TemplateHaskell
module Minicute.Utils.Common.TH
  ( qqRawCode

  , normalizeCode
  ) where

import Prelude hiding ( fail )

import Control.Monad.Fail
import Data.Char
import Data.List
import Data.String.Minicute
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

-- |
-- parse quoted string as an indented code text.
qqRawCode :: QuasiQuoter
qqRawCode
  = QuasiQuoter
    { quoteExp = qqRawCodeExp
    , quotePat = const $ fail "qqRawCode cannot be used as a pattern"
    , quoteType = const $ fail "qqRawCode cannot be used as a type"
    , quoteDec = const $ fail "qqRawCode cannot be used as a declaration"
    }
{-# INLINABLE qqRawCode #-}


qqRawCodeExp :: String -> Q Exp
qqRawCodeExp = lift . normalizeCode
{-# INLINE qqRawCodeExp #-}

normalizeCode :: String -> String
normalizeCode = updateString . toUnix
  where
    updateString
      = intercalate "\n"
        . adjustIndent
        . trimEndEmptyLines . trimStartEmptyLines
        . lines

    trimStartEmptyLines = dropWhile isEmpty
    trimEndEmptyLines = dropWhileEnd isEmpty

    isEmpty = all isSpace

    adjustIndent ls = drop indent <$> ls
      where
        indent = minimum $ length . takeWhile isSpace <$> ls

    {-# INLINABLE updateString #-}
    {-# INLINE trimStartEmptyLines #-}
    {-# INLINE trimEndEmptyLines #-}
    {-# INLINE isEmpty #-}
    {-# INLINABLE adjustIndent #-}
{-# INLINABLE normalizeCode #-}
