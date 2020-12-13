module AOC.Problem07Spec (spec) where

import AOC.Problem07
import SpecHelper

spec :: Spec
spec = do
  describe "problem 7" $ do
    context "part 1" $ do
      it "should calculate the number of bag colors that can contain a shiny gold bag" $ do
        runTestCases
          part1
          [ TestCase "input/07/0" 370,
            TestCase "input/07/1" 4
          ]
    context "part 2" $ do
      it "should calculate the number of bags required inside a single shiny gold bag" $ do
        runTestCases
          part2
          [ TestCase "input/07/0" 29547,
            TestCase "input/07/1" 32,
            TestCase "input/07/2" 126
          ]
