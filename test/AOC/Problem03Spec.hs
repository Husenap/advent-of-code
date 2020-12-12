module AOC.Problem03Spec (spec) where

import AOC.Problem03
import SpecHelper

spec :: Spec
spec = do
  describe "problem 3" $ do
    context "part 1" $ do
      it "should calculate the number of tree collisions for one slope" $ do
        contents <- readFile "input/3"
        part1 contents `shouldBe` 252
    context "part 2" $ do
      it "should calculate the product of tree collisions of multiple slopes" $ do
        contents <- readFile "input/3"
        part2 contents `shouldBe` 2608962048
