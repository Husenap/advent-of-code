module AOC.Problem12Spec (spec) where

import AOC.Problem12
import SpecHelper

spec :: Spec
spec = do
  describe "problem 12" $ do
    context "part 1" $ do
      it "should calculate the manhattan distance of the ships location after all actions" $ do
        runTestCases
          part1
          [ TestCase "input/12/0" 1645,
            TestCase "input/12/1" 25
          ]
    context "part 2" $ do
      it "should calculate the manhattan distance of the ships location after all actions" $ do
        runTestCases
          part2
          [ TestCase "input/12/0" 35292,
            TestCase "input/12/1" 286
          ]
