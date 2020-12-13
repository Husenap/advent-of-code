module AOC.Problem05Spec (spec) where

import AOC.Problem05
import SpecHelper

spec :: Spec
spec = do
  describe "problem 5" $ do
    context "part 1" $ do
      it "should calculate the maximum Seat Id" $ do
        runTestCases
          part1
          [ TestCase "input/05/0" 806
          ]
    context "part 2" $ do
      it "should calculate our Seat Id" $ do
        runTestCases
          part2
          [ TestCase "input/05/0" 562
          ]
