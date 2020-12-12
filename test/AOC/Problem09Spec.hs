module AOC.Problem09Spec (spec) where

import AOC.Problem09
import SpecHelper

spec :: Spec
spec = do
  describe "problem 9" $ do
    context "part 1" $ do
      it "should find the number that breaks the sum rule" $ do
        contents <- readFile "input/9"
        part1 contents 25 `shouldBe` 15690279
    context "part 2" $ do
      it "should find the encryption weakness" $ do
        contents <- readFile "input/9"
        part2 contents 25 `shouldBe` 2174232
