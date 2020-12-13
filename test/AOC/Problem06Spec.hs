module AOC.Problem06Spec (spec) where

import AOC.Problem06
import SpecHelper

spec :: Spec
spec = do
  describe "problem 6" $ do
    context "part 1" $ do
      it "should calculate the sum of all group unions" $ do
        runTestCases
          part1
          [ TestCase "input/06/0" 6170,
            TestCase "input/06/1" 11
          ]
    context "part 2" $ do
      it "should calculate the sum of all group intersections" $ do
        runTestCases
          part2
          [ TestCase "input/06/0" 2947,
            TestCase "input/06/1" 6
          ]
