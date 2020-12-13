module AOC.Problem11Spec (spec) where

import AOC.Problem11
import SpecHelper

spec :: Spec
spec = do
  describe "problem 11" $ do
    context "part 1" $ do
      it "should calculate the number of occupied seats after the simulation" $ do
        runTestCases
          part1
          [ TestCase "input/11/0" 2273,
            TestCase "input/11/1" 37
          ]