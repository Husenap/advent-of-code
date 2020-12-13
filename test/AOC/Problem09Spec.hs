module AOC.Problem09Spec (spec) where

import AOC.Problem09
import SpecHelper

spec :: Spec
spec = do
  describe "problem 9" $ do
    context "part 1" $ do
      it "should find the number that breaks the sum rule" $ do
        runTestCase (part1 25) (TestCase "input/09/0" 15690279)
        runTestCase (part1 5) (TestCase "input/09/1" 127)
    context "part 2" $ do
      it "should find the encryption weakness" $ do
        runTestCase (part2 25) (TestCase "input/09/0" 2174232)
        runTestCase (part2 5) (TestCase "input/09/1" 62)
