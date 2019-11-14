{- HLINT ignore "Redundant do" -}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Minicute.Transpilers.FreeVariablesSpec
  ( spec
  ) where

import Test.Hspec

import Control.Monad
import Data.Tuple.Extra
import Minicute.Data.Minicute.Annotated
import Minicute.Transpilers.FreeVariables
import Minicute.Utils.Minicute.TH

import qualified Data.Set as Set

spec :: Spec
spec = do
  describe "formFreeVariablesMainMC" $ do
    forM_ testCases (uncurry3 formFreeVariablesMainMCTest)

formFreeVariablesMainMCTest :: TestName -> TestBeforeContent -> TestAfterContent -> SpecWith (Arg Expectation)
formFreeVariablesMainMCTest name beforeContent afterContent = do
  it ("finds free variables for expressions in " <> name) $ do
    formFreeVariablesMainMC beforeContent `shouldBe` afterContent

type TestName = String
type TestBeforeContent = MainProgramMC
type TestAfterContent = ProgramMCWithFreeVariables Identifier
type TestCase = (TestName, TestBeforeContent, TestAfterContent)

-- |
-- __TODO: Introduce quosiquoter for 'AnnotatedProgramMC' and update__
-- __these test cases__
testCases :: [TestCase]
testCases =
  [ ( "empty program"
    , [qqMiniMainMC||]
    , Program
      []
    )

  , ( "program of a top-level definition with a single argument"
    , [qqMiniMainMC|
                  f x = x + x
      |]
    , Program
      [ Supercombinator
        ( "f"
        , ["x"]
        , AEApplication2
          (Set.singleton "x")
          (Set.singleton "x")
          (AEVariable Set.empty "+")
          (AEVariable (Set.singleton "x") "x")
          (AEVariable (Set.singleton "x") "x")
        )
      ]
    )

  , ( "program of a top-level definition of a let expression with a single let definition"
    , [qqMiniMainMC|
                  f = let
                        g = h 4
                      in
                        g
      |]
    , Program
      [ Supercombinator
        ( "f"
        , []
        , AELet
          Set.empty
          NonRecursive
          [ LetDefinition
            ( "g"
            , AEApplication Set.empty (AEVariable Set.empty "h") (AEInteger Set.empty 4)
            )
          ]
          (AEVariable (Set.singleton "g") "g")
        )
      ]
    )

  , ( "program of a top-level definition of a let expression with multiple let definitions"
    , [qqMiniMainMC|
                  f = let
                        g1 = h 4;
                        g2 = h 8;
                        g3 = 8 - 4
                      in
                        (g1 * g2) / g3
      |]
    , Program
      [ Supercombinator
        ( "f"
        , []
        , AELet
          Set.empty
          NonRecursive
          [ LetDefinition
            ( "g1"
            , AEApplication Set.empty (AEVariable Set.empty "h") (AEInteger Set.empty 4)
            )
          , LetDefinition
            ( "g2"
            , AEApplication Set.empty (AEVariable Set.empty "h") (AEInteger Set.empty 8)
            )
          , LetDefinition
            ( "g3"
            , AEApplication2 Set.empty Set.empty (AEVariable Set.empty "-") (AEInteger Set.empty 8) (AEInteger Set.empty 4)
            )
          ]
          ( AEApplication2
            (Set.fromList ["g1", "g2", "g3"])
            (Set.fromList ["g1", "g2"])
            (AEVariable Set.empty "/")
            ( AEApplication2
              (Set.fromList ["g1", "g2"])
              (Set.singleton "g1")
              (AEVariable Set.empty "*")
              (AEVariable (Set.singleton "g1") "g1")
              (AEVariable (Set.singleton "g2") "g2")
            )
            (AEVariable (Set.singleton "g3") "g3")
          )
        )
      ]
    )

  , ( "program of a top-level definition of a match expression"
    , [qqMiniMainMC|
                  f x = match x with
                          <1> -> 4;
                          <2> h t -> h + f t
      |]
    , Program
      [ Supercombinator
        ( "f"
        , ["x"]
        , AEMatch
          (Set.singleton "x")
          (AEVariable (Set.singleton "x") "x")
          [ MatchCase
            ( 1
            , []
            , AEInteger Set.empty 4
            )
          , MatchCase
            ( 2
            , ["h", "t"]
            , AEApplication2
              (Set.fromList ["h", "t"])
              (Set.singleton "h")
              (AEVariable Set.empty "+")
              (AEVariable (Set.singleton "h") "h")
              (AEApplication (Set.singleton "t") (AEVariable Set.empty "f") (AEVariable (Set.singleton "t") "t"))
            )
          ]
        )
      ]
    )

  , ( "program of a top-level definition of a lambda expression"
    , [qqMiniMainMC|
                  f = \x -> 4 + x
      |]
    , Program
      [ Supercombinator
        ( "f"
        , []
        , AELambda
          Set.empty
          ["x"]
          ( AEApplication2
            (Set.singleton "x")
            Set.empty
            (AEVariable Set.empty "+")
            (AEInteger Set.empty 4)
            (AEVariable (Set.singleton "x") "x")
          )
        )
      ]
    )
  ]
