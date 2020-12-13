module AOC.Problem03Spec (spec) where

import AOC.Problem03
import SpecHelper

spec :: Spec
spec = do
  describe "problem 3" $ do
    context "part 1" $ do
      it "should calculate the number of tree collisions for one slope" $ do
        runTestCases
          part1
          [ TestCase "input/03/0" 252,
            TestCase "input/03/1" 7
          ]
    context "part 2" $ do
      it "should calculate the product of tree collisions of multiple slopes" $ do
        runTestCases
          part2
          [ TestCase "input/03/0" 2608962048,
            TestCase "input/03/1" 336
          ]
