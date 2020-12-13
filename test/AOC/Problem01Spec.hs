module AOC.Problem01Spec (spec) where

import AOC.Problem01
import SpecHelper

spec :: Spec
spec = do
  describe "problem 1" $ do
    context "part 1" $ do
      it "should calculate the product of a pair" $ do
        runTestCases
          part1
          [ TestCase "input/01/0" 605364,
            TestCase "input/01/1" 514579
          ]
    context "part 2" $ do
      it "should calculate the product of a triplet" $ do
        runTestCases
          part2
          [ TestCase "input/01/0" 128397680,
            TestCase "input/01/1" 241861950
          ]
