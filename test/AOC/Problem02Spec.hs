module AOC.Problem02Spec where

import AOC.Problem02
import SpecHelper

spec :: Spec
spec = do
  describe "problem 2" $ do
    context "part 1" $ do
      it "should calculate the number of normal passwords" $ do
        contents <- readFile "input/2"
        part1 contents `shouldBe` 477
    context "part 2" $ do
      it "should calculate the number of toboggan passwords" $ do
        contents <- readFile "input/2"
        part2 contents `shouldBe` 686
