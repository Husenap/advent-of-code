module AOC.Problem10Spec where

import AOC.Problem10
import SpecHelper

spec :: Spec
spec = do
  describe "problem 10" $ do
    context "part 1" $ do
      it "should calculate the product of the differences in 1 and 3 jolts" $ do
        contents <- readFile "input/10"
        part1 contents `shouldBe` 1820
    context "part 2" $ do
      it "should calculate the number of permutations that will result in a valid chain" $ do
        contents1 <- readFile "input/10"
        part2 contents1 `shouldBe` 3454189699072
