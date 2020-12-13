module AOC.Problem13Spec (spec) where

import AOC.Problem13
import SpecHelper

spec :: Spec
spec = do
  describe "problem 13" $ do
    context "part 1" $ do
      it "should calculate the earliest bus to take" $ do
        runTestCases
          part1
          [ TestCase "input/13/0" 6559
          ]
    context "part 2" $ do
      it "should calculate the earliest timestamp such that all of the listed bus IDs depart at offsets matching their positions in the list" $ do
        runTestCases
          part2
          [ TestCase "input/13/0" 626670513163231,
            TestCase "input/13/1" 1068781,
            TestCase "input/13/2" 3417,
            TestCase "input/13/3" 754018,
            TestCase "input/13/4" 779210,
            TestCase "input/13/5" 1261476,
            TestCase "input/13/6" 1202161486
          ]
