module AOC.Problem04Spec (spec) where

import AOC.Problem04
import SpecHelper

spec :: Spec
spec = do
  describe "problem 4" $ do
    context "part 1" $ do
      it "should calculate the number of passports with correct fields" $ do
        runTestCases
          part1
          [ TestCase "input/04/0" 247,
            TestCase "input/04/1" 2,
            TestCase "input/04/2" 8
          ]
    context "part 2" $ do
      it "should calculate the number of valid passports" $ do
        runTestCases
          part2
          [ TestCase "input/04/0" 145,
            TestCase "input/04/1" 2,
            TestCase "input/04/2" 4
          ]
