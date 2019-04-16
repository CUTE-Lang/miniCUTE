module Minicute.Parser.ParserSpec
  ( spec
  ) where

import Test.Hspec
import Test.Hspec.Megaparsec

import Control.Monad
import Minicute.Types.Program
import Text.Megaparsec

import qualified Minicute.Parser.Parser as P

spec :: Spec
spec
  = do
  describe "programL parser" $ do
    it "parses valid programs successfullly" $ do
      forM_ cases (uncurry programLTest)

programLTest :: String -> MainProgramL -> Expectation
programLTest = shouldParse . parse P.programL ""

cases :: [(String, MainProgramL)]
cases = []
