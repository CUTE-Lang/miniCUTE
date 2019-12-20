module Data.Text.Prettyprint.Doc.Minicute.Internal.TH
  ( makePrettyMCFromPretty
  ) where

import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax

makePrettyMCFromPretty :: Name -> DecsQ
makePrettyMCFromPretty tName = pure <$> prettyMCInstance
  where
    prettyMCFunctionName = mkName "prettyMC"
    prettyListMCFunctionName = mkName "prettyListMC"
    prettyMCClassName = mkName "Data.Text.Prettyprint.Doc.Minicute.PrettyMC"

    prettyMCInstance
      = instanceD (cxt []) (conT prettyMCClassName `appT` conT tName)
        [ funD prettyMCFunctionName
          [clause [wildP] (normalB (varE (mkName "Data.Text.Prettyprint.Doc.pretty"))) []]
        , funD prettyListMCFunctionName
          [clause [wildP] (normalB (varE (mkName "Data.Text.Prettyprint.Doc.prettyList"))) []]
        , pragInlD prettyMCFunctionName Inlinable FunLike AllPhases
        , pragInlD prettyListMCFunctionName Inlinable FunLike AllPhases
        ]
