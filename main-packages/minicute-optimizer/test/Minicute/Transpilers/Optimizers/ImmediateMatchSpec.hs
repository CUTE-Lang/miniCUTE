{- HLINT ignore "Redundant do" -}
{-# LANGUAGE QuasiQuotes #-}
-- |
-- Copyright: (c) 2018-present Junyoung Clare Jang
-- License: BSD 3-Clause
module Minicute.Transpilers.Optimizers.ImmediateMatchSpec
  ( spec
  ) where

import Test.Hspec

import Control.Monad
import Data.Tuple.Extra
import Minicute.Data.Minicute.Program
import Minicute.Transpilers.Optimizers.ImmediateMatch
import Minicute.Utils.Minicute.TH

spec :: Spec
spec = do
  describe "immediateMatchMainMC" $ do
    forM_ testCases (uncurry3 immediateMatchMainMCTest)

immediateMatchMainMCTest :: TestName -> TestBeforeContent -> TestAfterContent -> SpecWith (Arg Expectation)
immediateMatchMainMCTest name beforeContent afterContent = do
  it ("immediate matchs are optimized in " <> name) $ do
    immediateMatchMainMC beforeContent `shouldBe` afterContent

type TestName = String
type TestBeforeContent = MainProgramMC
type TestAfterContent = MainProgramMC
type TestCase = (TestName, TestBeforeContent, TestAfterContent)

testCases :: [TestCase]
testCases
  = [ ( "program with a no argument constructor"
      , [qqMiniMainMC|
                    f = let
                          x = $C{1;0}
                        in
                          match x with
                            <1> -> 5
        |]
      , [qqMiniMainMC|
                    f = let
                          x = $C{1;0}
                        in
                          5
        |]
      )

    , ( "program with a two-argument constructor"
      , [qqMiniMainMC|
                    f = let
                          x = $C{2;2} 1 $C{1;0}
                        in
                          match x with
                            <1> -> 5;
                            <2> h t -> h
        |]
      , [qqMiniMainMC|
                    f = let
                          x = $C{2;2} 1 $C{1;0}
                        in
                          let
                            h = 1;
                            t = $C{1;0}
                          in
                            h
        |]
      )

    , ( "program with nested optimizable matches"
      , [qqMiniMainMC|
                    f = let
                          x = $C{2;2} 1 $C{1;0}
                        in
                          match x with
                            <1> -> 5;
                            <2> h t ->
                              match t with
                                <1> -> h;
                                <2> th tt -> h + th
        |]
      , [qqMiniMainMC|
                    f = let
                          x = $C{2;2} 1 $C{1;0}
                        in
                          let
                            h = 1;
                            t = $C{1;0}
                          in
                            h
        |]
      )
    ]
