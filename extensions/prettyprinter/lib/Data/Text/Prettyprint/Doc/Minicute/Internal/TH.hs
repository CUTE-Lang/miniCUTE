module Data.Text.Prettyprint.Doc.Minicute.Internal.TH
  ( makePrettyMCFromPretty
  ) where

import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax

-- |
-- 'makePrettyMCFromPretty' makes an instance of
-- 'Data.Text.Prettyprint.Doc.Minicute.PrettyMC' class using an instance of
-- 'Data.Text.Prettyprint.Pretty' class.
-- If the target type does not have an instance of 'Data.Text.Prettyprint.Pretty' class,
-- this function emits an error.
makePrettyMCFromPretty :: Name -> DecsQ
makePrettyMCFromPretty tName = pure <$> prettyMCInstance
  where
    prettyMCFunctionName = mkName "prettyMC"
    prettyMCClassName = mkName "Data.Text.Prettyprint.Doc.Minicute.PrettyMC"

    prettyMCInstance
      = instanceD (cxt []) (conT prettyMCClassName `appT` conT tName)
        [ funD prettyMCFunctionName
          [clause [wildP] (normalB (varE (mkName "Data.Text.Prettyprint.Doc.pretty"))) []]
        , pragInlD prettyMCFunctionName Inline FunLike AllPhases
        ]

    {-# INLINE prettyMCClassName #-}
    {-# INLINE prettyMCInstance #-}
