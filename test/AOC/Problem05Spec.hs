module AOC.Problem05Spec (spec) where

import AOC.Problem05
import SpecHelper

spec :: Spec
spec = do
  describe "problem 5" $ do
    context "part 1" $ do
      it "should calculate the maximum Seat Id" $ do
        contents <- readFile "input/5"
        part1 contents `shouldBe` 806
    context "part 2" $ do
      it "should calculate our Seat Id" $ do
        contents <- readFile "input/5"
        part2 contents `shouldBe` 562
