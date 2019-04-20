module Minicute.PrettyPrintableSpec
  ( spec
  ) where

import Test.Hspec
import Test.Hspec.Megaparsec

import Control.Monad
import Data.Tuple.Extra
import Minicute.Types.Minicute.Program
import Text.Megaparsec

import qualified Minicute.Data.PrintSequence as PS
import qualified Minicute.PrettyPrintable as PP
import qualified Minicute.Parser.Parser as P

spec :: Spec
spec = do
  describe "prettyPrint" $ do
    forM_ testCases (uncurry3 programLTest)

programLTest :: TestName -> TestContent -> TestExpected -> SpecWith (Arg Expectation)
programLTest name program expected = do
  it ("prints re-parsable text for " <> name) $ do
    parse P.programL "" (PS.toString (PP.prettyPrint program)) `shouldParse` program
  it ("prints expected text for " <> name) $ do
    PS.toString (PP.prettyPrint program) `shouldBe` expected

type TestName = String
type TestContent = MainProgramL
type TestExpected = String
type TestCase = (TestName, TestContent, TestExpected)

testCases :: [TestCase]
testCases
  = [ ( "empty program"
      , ProgramL
        [
        ]
      , ""
      )
    , ( "simple program"
      , ProgramL
        [ ( "f"
          , []
          , ELInteger 5
          )
        ]
      , "f = 5"
      )
    , ( "program with multiple top-level definitions"
      , ProgramL
        [ ( "f"
          , []
          , ELVariable "g"
          )
        , ( "g"
          , []
          , ELInteger 5
          )
        ]
      , "f = g;\ng = 5"
      )
    ]
