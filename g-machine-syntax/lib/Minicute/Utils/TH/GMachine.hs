-- |
-- G-machine syntax utilities for miniCUTE compiler using TemplateHaskell
module Minicute.Utils.TH.GMachine
  ( qqGMachine

  , qqRawCode
  ) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Minicute.Parser.GMachine.Parser
import Minicute.Utils.TH
import Text.Megaparsec

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


qqGMachineExp :: String -> Q Exp
qqGMachineExp = lift . either (error . errorBundlePretty) id . parse gMachineProgram "" . normalizeCode
