-- |
-- Copyright: (c) 2018-present Junyoung Clare Jang
-- License: BSD 3-Clause
--
-- String functions used in miniCUTE compiler
module Data.String.Minicute
  ( toUnix
  ) where

-- |
-- Normalize a multi-line input string into a unix-style multi-line string.
--
-- >>> toUnix "\r\n"
-- "\n"
-- >>> toUnix "\r"
-- "\n"
-- >>> toUnix "abcde\r\nfghij\n"
-- "abcde\nfghij\n"
toUnix :: String -> String
toUnix ('\r' : '\n' : cs) = '\n' : toUnix cs
toUnix ('\r' : cs) = '\n' : toUnix cs
toUnix (c : cs) = c : toUnix cs
toUnix [] = []
{-# INLINABLE toUnix #-}
