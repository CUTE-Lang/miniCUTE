{- HLINT ignore "Redundant do" -}
module Minicute.Transpiler.FreeVariablesSpec
  ( spec
  ) where

import Test.Hspec

import Control.Monad
import Data.Tuple.Extra
import Minicute.Transpiler.FreeVariables
import Minicute.Types.Minicute.Program

import qualified Data.Set as Set

spec :: Spec
spec = do
  describe "formFreeVariablesMainL" $ do
    forM_ testCases (uncurry3 formFreeVariablesMainLTest)

formFreeVariablesMainLTest :: TestName -> TestBeforeContent -> TestAfterContent -> SpecWith (Arg Expectation)
formFreeVariablesMainLTest name beforeContent afterContent = do
  it ("finds free variables for expressions in " <> name) $ do
    formFreeVariablesMainL beforeContent `shouldBe` afterContent

type TestName = String
type TestBeforeContent = MainProgramL
type TestAfterContent = ProgramLWithFreeVariables Identifier
type TestCase = (TestName, TestBeforeContent, TestAfterContent)

testCases :: [TestCase]
testCases =
  [ ( "empty program"
    , ProgramL
      []
    , AnnotatedProgramL
      []
    )

  , ( "program of a top-level definition with a single argument"
    , ProgramL
      [ ( "f"
        , ["x"]
        , ELApplication2 (ELVariable "+") (ELVariable "x") (ELVariable "x")
        )
      ]
    , AnnotatedProgramL
      [ ( "f"
        , ["x"]
        , AELApplication2 (Set.singleton "x") (Set.singleton "x") (AELVariable Set.empty "+") (AELVariable (Set.singleton "x") "x") (AELVariable (Set.singleton "x") "x")
        )
      ]
    )

  , ( "program of a top-level definition of a let expression with a single let definition"
    , ProgramL
      [ ( "f"
        , []
        , ELLet
          NonRecursive
          [ ( "g"
            , ELApplication (ELVariable "h") (ELInteger 4)
            )
          ]
          (ELVariable "g")
        )
      ]
    , AnnotatedProgramL
      [ ( "f"
        , []
        , AELLet
          Set.empty
          NonRecursive
          [ ( "g"
            , AELApplication Set.empty (AELVariable Set.empty "h") (AELInteger Set.empty 4)
            )
          ]
          (AELVariable (Set.singleton "g") "g")
        )
      ]
    )

  , ( "program of a top-level definition of a let expression with multiple let definitions"
    , ProgramL
      [ ( "f"
        , []
        , ELLet
          NonRecursive
          [ ( "g1"
            , ELApplication (ELVariable "h") (ELInteger 4)
            )
          , ( "g2"
            , ELApplication (ELVariable "h") (ELInteger 8)
            )
          , ( "g3"
            , ELApplication2 (ELVariable "-") (ELInteger 8) (ELInteger 4)
            )
          ]
          ( ELApplication2
            (ELVariable "/")
            ( ELApplication2
              (ELVariable "*")
              (ELVariable "g1")
              (ELVariable "g2")
            )
            (ELVariable "g3")
          )
        )
      ]
    , AnnotatedProgramL
      [ ( "f"
        , []
        , AELLet
          Set.empty
          NonRecursive
          [ ( "g1"
            , AELApplication Set.empty (AELVariable Set.empty "h") (AELInteger Set.empty 4)
            )
          , ( "g2"
            , AELApplication Set.empty (AELVariable Set.empty "h") (AELInteger Set.empty 8)
            )
          , ( "g3"
            , AELApplication2 Set.empty Set.empty (AELVariable Set.empty "-") (AELInteger Set.empty 8) (AELInteger Set.empty 4)
            )
          ]
          ( AELApplication2
            (Set.fromList ["g1", "g2", "g3"])
            (Set.fromList ["g1", "g2"])
            (AELVariable Set.empty "/")
            ( AELApplication2
              (Set.fromList ["g1", "g2"])
              (Set.singleton "g1")
              (AELVariable Set.empty "*")
              (AELVariable (Set.singleton "g1") "g1")
              (AELVariable (Set.singleton "g2") "g2")
            )
            (AELVariable (Set.singleton "g3") "g3")
          )
        )
      ]
    )

  , ( "program of a top-level definition of a match expression"
    , ProgramL
      [ ( "f"
        , ["x"]
        , ELMatch
          (ELVariable "x")
          [ ( 1
            , []
            , ELInteger 4
            )
          , ( 2
            , ["h", "t"]
            , ELApplication2
              (ELVariable "+")
              (ELVariable "h")
              (ELApplication (ELVariable "f") (ELVariable "t"))
            )
          ]
        )
      ]
    , AnnotatedProgramL
      [ ( "f"
        , ["x"]
        , AELMatch
          (Set.singleton "x")
          (AELVariable (Set.singleton "x") "x")
          [ ( 1
            , []
            , AELInteger Set.empty 4
            )
          , ( 2
            , ["h", "t"]
            , AELApplication2
              (Set.fromList ["h", "t"])
              (Set.singleton "h")
              (AELVariable Set.empty "+")
              (AELVariable (Set.singleton "h") "h")
              (AELApplication (Set.singleton "t") (AELVariable Set.empty "f") (AELVariable (Set.singleton "t") "t"))
            )
          ]
        )
      ]
    )

  , ( "program of a top-level definition of a lambda expression"
    , ProgramL
      [ ( "f"
        , []
        , ELLambda ["x"] (ELApplication2 (ELVariable "+") (ELInteger 4) (ELVariable "x"))
        )
      ]
    , AnnotatedProgramL
      [ ( "f"
        , []
        , AELLambda Set.empty ["x"] (AELApplication2 (Set.singleton "x") Set.empty (AELVariable Set.empty "+") (AELInteger Set.empty 4) (AELVariable (Set.singleton "x") "x"))
        )
      ]
    )
  ]
