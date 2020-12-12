module AOC.Problem10 (part1, part2) where

import Data.List
import Data.MemoTrie

parseData :: String -> [Int]
parseData contents = do
  let numbers = map (read :: String -> Int) (lines contents)
  sort (numbers ++ [0, maximum numbers + 3]) -- add charging outlet and device's built-in adapter

part1 :: String -> Int
part1 contents = do
  let numbers = parseData contents
      differences = [b - a | a : b : _ <- tails numbers]
      d1 = length $ filter (== 1) differences
      d3 = length $ filter (== 3) differences
  d1 * d3

calculatePermutations :: [(Int, [Int])] -> Int -> Int
calculatePermutations = memo2 calc
  where
    calc connections current = case lookup current connections of
      Just incoming -> sum [calculatePermutations connections next | next <- incoming]
      Nothing -> 1

part2 :: String -> Int
part2 contents = do
  let numbers = reverse $ parseData contents
      connections =
        [ (a, incoming)
          | a : b <- tails numbers,
            let incoming = takeWhile (\n -> a - n <= 3) b,
            not (null incoming)
        ]
      numberOfPermutations = calculatePermutations connections (maximum numbers)
  numberOfPermutations