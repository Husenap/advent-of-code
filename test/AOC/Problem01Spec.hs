module AOC.Problem01Spec (spec) where

import AOC.Problem01
import SpecHelper

spec :: Spec
spec = do
  describe "problem 1" $ do
    context "part 1" $ do
      it "should calculate the product of a pair" $ do
        contents <- readFile "input/1"
        part1 contents `shouldBe` 605364
    context "part 2" $ do
      it "should calculate the product of a triplet" $ do
        contents <- readFile "input/1"
        part2 contents `shouldBe` 128397680
