module AOC.Problem11Spec (spec) where

import AOC.Problem11
import SpecHelper

spec :: Spec
spec = do
  describe "problem 11" $ do
    context "part 1" $ do
      it "should calculate the number of occupied seats after the simulation" $ do
        contents <- readFile "input/11"
        part1 contents `shouldBe` 2273