module AOC.Problem02Spec (spec) where

import AOC.Problem02
import SpecHelper

spec :: Spec
spec = do
  describe "problem 2" $ do
    context "part 1" $ do
      it "should calculate the number of normal passwords" $ do
        runTestCases
          part1
          [ TestCase "input/02/0" 477,
            TestCase "input/02/1" 2
          ]
    context "part 2" $ do
      it "should calculate the number of toboggan passwords" $ do
        runTestCases
          part2
          [ TestCase "input/02/0" 686,
            TestCase "input/02/1" 1
          ]
