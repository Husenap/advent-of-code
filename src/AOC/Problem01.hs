module AOC.Problem01 (part1, part2) where

import Data.List

pairs :: Num b => [b] -> [(b, b)]
pairs l = [(x + y, x * y) | (x : ys) <- tails l, y <- ys]

triplets :: Num b => [b] -> [(b, b)]
triplets l = [(x + y + z, x * y * z) | (x : ys) <- tails l, (y : zs) <- tails ys, z <- zs]

calculateAnswer :: (Foldable t, Eq a, Num a) => String -> ([Int] -> t (a, b)) -> b
calculateAnswer contents f = do
  let numbers = map (read :: String -> Int) (lines contents)
      (Just (_, result)) = find (\a -> fst a == 2020) (f numbers)
  result

part1 :: String -> Int
part1 contents = calculateAnswer contents pairs

part2 :: String -> Int
part2 contents = calculateAnswer contents triplets
