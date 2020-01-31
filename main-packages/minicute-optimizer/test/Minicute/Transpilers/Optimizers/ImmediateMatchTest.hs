{- HLINT ignore "Redundant do" -}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
-- |
-- Copyright: (c) 2018-present Junyoung Clare Jang
-- License: BSD 3-Clause
module Minicute.Transpilers.Optimizers.ImmediateMatchTest
  ( spec_immediateMatchMain
  ) where

import Test.Tasty.Hspec

import Control.Monad
import Data.Tuple.Extra
import Minicute.Transpilers.Optimizers.ImmediateMatch
import Minicute.Utils.Minicute.TH

spec_immediateMatchMain :: Spec
spec_immediateMatchMain
  = forM_ testCases (uncurry3 immediateMatchMainTest)

immediateMatchMainTest :: TestName -> TestBeforeContent -> TestAfterContent -> SpecWith (Arg Expectation)
immediateMatchMainTest name beforeContent afterContent = do
  it ("immediate matchs are optimized in " <> name) $ do
    immediateMatchMain beforeContent `shouldBe` afterContent

type TestName = String
type TestBeforeContent = MainProgram 'Simple 'MC
type TestAfterContent = MainProgram 'Simple 'MC
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
