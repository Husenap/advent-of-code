module AOC.Problem08Spec (spec) where

import AOC.Problem08
import SpecHelper

spec :: Spec
spec = do
  describe "problem 8" $ do
    context "part 1" $ do
      it "should calculate the value of the accumulator before infinite loop" $ do
        contents <- readFile "input/8"
        part1 contents `shouldBe` 1384
    context "part 2" $ do
      it "should calculate the value of the accumulator after fixing the infinite loop" $ do
        contents <- readFile "input/8"
        part2 contents `shouldBe` 761
