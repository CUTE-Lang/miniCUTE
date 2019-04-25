module Minicute.Data.String where

toUnix :: String -> String
toUnix ('\r' : '\n' : cs) = '\n' : toUnix cs
toUnix ('\r' : cs) = '\n' : toUnix cs
toUnix (c : cs) = c : toUnix cs
toUnix [] = []
