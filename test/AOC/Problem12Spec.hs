module AOC.Problem12Spec (spec) where

import AOC.Problem12
import SpecHelper

spec :: Spec
spec = do
  describe "problem 12" $ do
    context "part 1" $ do
      it "should calculate the manhattan distance of the ships location after all actions" $ do
        contents <- readFile "input/12"
        part1 contents `shouldBe` 1645
    context "part 2" $ do
      it "should calculate the manhattan distance of the ships location after all actions" $ do
        contents <- readFile "input/12"
        part2 contents `shouldBe` 35292
