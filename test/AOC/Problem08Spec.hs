module AOC.Problem08Spec (spec) where

import AOC.Problem08
import SpecHelper

spec :: Spec
spec = do
  describe "problem 8" $ do
    context "part 1" $ do
      it "should calculate the value of the accumulator before infinite loop" $ do
        runTestCases
          part1
          [ TestCase "input/08/0" 1384,
            TestCase "input/08/1" 5
          ]
    context "part 2" $ do
      it "should calculate the value of the accumulator after fixing the infinite loop" $ do
        runTestCases
          part2
          [ TestCase "input/08/0" 761,
            TestCase "input/08/1" 8
          ]
