{- HLINT ignore "Redundant do" -}
{-# LANGUAGE QuasiQuotes #-}
module Minicute.Transpilers.Optimizers.ImmediateMatchSpec
  ( spec
  ) where

import Test.Hspec

import Control.Monad
import Data.Tuple.Extra
import Minicute.Transpilers.Optimizers.ImmediateMatch
import Minicute.Data.Minicute.Program
import Minicute.Utils.TH

spec :: Spec
spec = do
  describe "immediateMatchMainL" $ do
    forM_ testCases (uncurry3 immediateMatchMainLTest)

immediateMatchMainLTest :: TestName -> TestBeforeContent -> TestAfterContent -> SpecWith (Arg Expectation)
immediateMatchMainLTest name beforeContent afterContent = do
  it ("immediate matchs are optimized in " <> name) $ do
    immediateMatchMainL beforeContent `shouldBe` afterContent

type TestName = String
type TestBeforeContent = MainProgramL
type TestAfterContent = MainProgramL
type TestCase = (TestName, TestBeforeContent, TestAfterContent)

testCases :: [TestCase]
testCases
  = [ ( "program with a no argument constructor"
      , [qqMiniMainL|
                    f = let
                          x = $C{1;0}
                        in
                          match x with
                            <1> -> 5
        |]
      , [qqMiniMainL|
                    f = let
                          x = $C{1;0}
                        in
                          5
        |]
      )

    , ( "program with a two-argument constructor"
      , [qqMiniMainL|
                    f = let
                          x = $C{2;2} 1 $C{1;0}
                        in
                          match x with
                            <1> -> 5;
                            <2> h t -> h
        |]
      , [qqMiniMainL|
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
      , [qqMiniMainL|
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
      , [qqMiniMainL|
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
