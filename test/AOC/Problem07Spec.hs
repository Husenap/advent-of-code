module AOC.Problem07Spec where

import AOC.Problem07
import SpecHelper

spec :: Spec
spec = do
  describe "problem 7" $ do
    context "part 1" $ do
      it "should calculate the number of bag colors that can contain a shiny gold bag" $ do
        contents <- readFile "input/7"
        part1 contents `shouldBe` 370
    context "part 2" $ do
      it "should calculate the number of bags required inside a single shiny gold bag" $ do
        contents <- readFile "input/7"
        part2 contents `shouldBe` 29547
