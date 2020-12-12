module AOC.Problem04Spec (spec) where

import AOC.Problem04
import SpecHelper

spec :: Spec
spec = do
  describe "problem 4" $ do
    context "part 1" $ do
      it "should calculate the number of passports with correct fields" $ do
        contents <- readFile "input/4"
        part1 contents `shouldBe` 247
    context "part 2" $ do
      it "should calculate the number of valid passports" $ do
        contents <- readFile "input/4"
        part2 contents `shouldBe` 145
