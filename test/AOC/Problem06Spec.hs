module AOC.Problem06Spec where

import AOC.Problem06
import SpecHelper

spec :: Spec
spec = do
  describe "problem 6" $ do
    context "part 1" $ do
      it "should calculate the sum of all group unions" $ do
        contents <- readFile "input/6"
        part1 contents `shouldBe` 6170
    context "part 2" $ do
      it "should calculate the sum of all group intersections" $ do
        contents <- readFile "input/6"
        part2 contents `shouldBe` 2947
