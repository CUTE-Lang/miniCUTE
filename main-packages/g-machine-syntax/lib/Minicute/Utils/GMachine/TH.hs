-- |
-- Copyright: (c) 2018-present Junyoung Clare Jang
-- License: BSD 3-Clause
--
-- G-machine syntax utilities for miniCUTE compiler using TemplateHaskell
module Minicute.Utils.GMachine.TH
  ( qqGMachine

  , qqRawCode
  ) where

import Prelude hiding ( fail )

import Control.Monad.Fail
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Minicute.Parser.GMachine.Parser
import Minicute.Utils.Common.TH
import Text.Megaparsec

-- |
-- parse quoted string as a 'GMachineProgram'.
qqGMachine :: QuasiQuoter
qqGMachine
  = QuasiQuoter
    { quoteExp = qqGMachineExp
    , quotePat = const $ fail "qqMiniMainExp cannot be used as a pattern"
    , quoteType = const $ fail "qqMiniMainExp cannot be used as a type"
    , quoteDec = const $ fail "qqMiniMainExp cannot be used as a declaration"
    }
{-# INLINABLE qqGMachine #-}


qqGMachineExp :: String -> Q Exp
qqGMachineExp
  = either (fail . errorBundlePretty) lift
    . parse gMachineProgram ""
    . normalizeCode
{-# INLINABLE qqGMachineExp #-}
