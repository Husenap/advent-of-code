module AOC.Problem10Spec where

import AOC.Problem10
import SpecHelper

spec :: Spec
spec = do
  describe "problem 10" $ do
    context "part 1" $ do
      it "should calculate the product of the differences in 1 and 3 jolts" $ do
        runTestCases
          part1
          [ TestCase "input/10/0" 1820,
            TestCase "input/10/1" 35,
            TestCase "input/10/2" 220
          ]
    context "part 2" $ do
      it "should calculate the number of permutations that will result in a valid chain" $ do
        runTestCases
          part2
          [ TestCase "input/10/0" 3454189699072,
            TestCase "input/10/1" 8,
            TestCase "input/10/2" 19208
          ]
