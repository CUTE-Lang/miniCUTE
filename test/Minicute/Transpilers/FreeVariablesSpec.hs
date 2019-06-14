{- HLINT ignore "Redundant do" -}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Minicute.Transpilers.FreeVariablesSpec
  ( spec
  ) where

import Test.Hspec

import Control.Monad
import Data.Tuple.Extra
import Minicute.Transpilers.FreeVariables
import Minicute.Types.Minicute.Annotated.Program
import Minicute.Utils.TH

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

-- |
-- __TODO: Introduce quosiquoter for 'AnnotatedProgramL' and update__
-- __these test cases__
testCases :: [TestCase]
testCases =
  [ ( "empty program"
    , [qqMiniMainL||]
    , AnnotatedProgramL
      []
    )

  , ( "program of a top-level definition with a single argument"
    , [qqMiniMainL|
                  f x = x + x
      |]
    , AnnotatedProgramL
      [ AnnotatedSupercombinatorL
        "f"
        ["x"]
        ( AELApplication2
          (Set.singleton "x")
          (Set.singleton "x")
          (AELVariable Set.empty "+")
          (AELVariable (Set.singleton "x") "x")
          (AELVariable (Set.singleton "x") "x")
        )
      ]
    )

  , ( "program of a top-level definition of a let expression with a single let definition"
    , [qqMiniMainL|
                  f = let
                        g = h 4
                      in
                        g
      |]
    , AnnotatedProgramL
      [ AnnotatedSupercombinatorL
        "f"
        []
        ( AELLet
          Set.empty
          NonRecursive
          [ AnnotatedLetDefinitionL
            "g"
            (AELApplication Set.empty (AELVariable Set.empty "h") (AELInteger Set.empty 4))
          ]
          (AELVariable (Set.singleton "g") "g")
        )
      ]
    )

  , ( "program of a top-level definition of a let expression with multiple let definitions"
    , [qqMiniMainL|
                  f = let
                        g1 = h 4;
                        g2 = h 8;
                        g3 = 8 - 4
                      in
                        (g1 * g2) / g3
      |]
    , AnnotatedProgramL
      [ AnnotatedSupercombinatorL
        "f"
        []
        ( AELLet
          Set.empty
          NonRecursive
          [ AnnotatedLetDefinitionL
            "g1"
            (AELApplication Set.empty (AELVariable Set.empty "h") (AELInteger Set.empty 4))
          , AnnotatedLetDefinitionL
            "g2"
            (AELApplication Set.empty (AELVariable Set.empty "h") (AELInteger Set.empty 8))
          , AnnotatedLetDefinitionL
            "g3"
            (AELApplication2 Set.empty Set.empty (AELVariable Set.empty "-") (AELInteger Set.empty 8) (AELInteger Set.empty 4))
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
    , [qqMiniMainL|
                  f x = match x with
                          <1> -> 4;
                          <2> h t -> h + f t
      |]
    , AnnotatedProgramL
      [ AnnotatedSupercombinatorL
        "f"
        ["x"]
        ( AELMatch
          (Set.singleton "x")
          (AELVariable (Set.singleton "x") "x")
          [ AnnotatedMatchCaseL
            1
            []
            (AELInteger Set.empty 4)
          , AnnotatedMatchCaseL
            2
            ["h", "t"]
            ( AELApplication2
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
    , [qqMiniMainL|
                  f = \x -> 4 + x
      |]
    , AnnotatedProgramL
      [ AnnotatedSupercombinatorL
        "f"
        []
        ( AELLambda
          Set.empty
          ["x"]
          ( AELApplication2
            (Set.singleton "x")
            Set.empty
            (AELVariable Set.empty "+")
            (AELInteger Set.empty 4)
            (AELVariable (Set.singleton "x") "x")
          )
        )
      ]
    )
  ]
