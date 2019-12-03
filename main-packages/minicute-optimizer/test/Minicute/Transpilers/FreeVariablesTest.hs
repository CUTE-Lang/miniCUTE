{- HLINT ignore "Redundant do" -}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
-- |
-- Copyright: (c) 2018-present Junyoung Clare Jang
-- License: BSD 3-Clause
module Minicute.Transpilers.FreeVariablesTest
  ( spec_formFreeVariablesMainMC
  ) where

import Test.Tasty.Hspec

import Control.Monad
import Data.Tuple.Extra
import Minicute.Data.Minicute.Program
import Minicute.Transpilers.FreeVariables
import Minicute.Utils.Minicute.TH

import qualified Data.Set as Set

spec_formFreeVariablesMainMC :: Spec
spec_formFreeVariablesMainMC
  = forM_ testCases (uncurry3 formFreeVariablesMainMCTest)

formFreeVariablesMainMCTest :: TestName -> TestBeforeContent -> TestAfterContent -> SpecWith (Arg Expectation)
formFreeVariablesMainMCTest name beforeContent afterContent = do
  it ("finds free variables for expressions in " <> name) $ do
    formFreeVariablesMainMC beforeContent `shouldBe` afterContent

type TestName = String
type TestBeforeContent = MainProgram 'Simple 'MC
type TestAfterContent = MainProgram ('AnnotatedWith FreeVariables) 'MC
type TestCase = (TestName, TestBeforeContent, TestAfterContent)

-- |
-- __TODO: Introduce quosiquoter for an annotated program and update__
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
        , EApplication2
          (Set.singleton "x")
          (Set.singleton "x")
          (EPrimitive Set.empty PrimAdd)
          (EVariable (Set.singleton "x") "x")
          (EVariable (Set.singleton "x") "x")
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
        , ELet
          Set.empty
          NonRecursive
          [ LetDefinition
            ( "g"
            , EApplication Set.empty (EVariable Set.empty "h") (EInteger Set.empty 4)
            )
          ]
          (EVariable (Set.singleton "g") "g")
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
        , ELet
          Set.empty
          NonRecursive
          [ LetDefinition
            ( "g1"
            , EApplication Set.empty (EVariable Set.empty "h") (EInteger Set.empty 4)
            )
          , LetDefinition
            ( "g2"
            , EApplication Set.empty (EVariable Set.empty "h") (EInteger Set.empty 8)
            )
          , LetDefinition
            ( "g3"
            , EApplication2 Set.empty Set.empty (EPrimitive Set.empty PrimSub) (EInteger Set.empty 8) (EInteger Set.empty 4)
            )
          ]
          ( EApplication2
            (Set.fromList ["g1", "g2", "g3"])
            (Set.fromList ["g1", "g2"])
            (EPrimitive Set.empty PrimDiv)
            ( EApplication2
              (Set.fromList ["g1", "g2"])
              (Set.singleton "g1")
              (EPrimitive Set.empty PrimMul)
              (EVariable (Set.singleton "g1") "g1")
              (EVariable (Set.singleton "g2") "g2")
            )
            (EVariable (Set.singleton "g3") "g3")
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
        , EMatch
          (Set.singleton "x")
          (EVariable (Set.singleton "x") "x")
          [ MatchCase
            ( 1
            , []
            , EInteger Set.empty 4
            )
          , MatchCase
            ( 2
            , ["h", "t"]
            , EApplication2
              (Set.fromList ["h", "t"])
              (Set.singleton "h")
              (EPrimitive Set.empty PrimAdd)
              (EVariable (Set.singleton "h") "h")
              (EApplication (Set.singleton "t") (EVariable Set.empty "f") (EVariable (Set.singleton "t") "t"))
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
        , ELambda
          Set.empty
          ["x"]
          ( EApplication2
            (Set.singleton "x")
            Set.empty
            (EPrimitive Set.empty PrimAdd)
            (EInteger Set.empty 4)
            (EVariable (Set.singleton "x") "x")
          )
        )
      ]
    )
  ]
