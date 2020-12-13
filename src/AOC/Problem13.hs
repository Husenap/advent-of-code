module AOC.Problem13 (part1, part2) where

import Data.List.Split

parseDataPart1 :: String -> (Int, [Int])
parseDataPart1 contents = (time, busses)
  where
    rows = lines contents
    time = read $ head rows
    busses = map read $ filter (/= "x") $ splitOn "," $ last rows

part1 :: String -> Int
part1 contents = do
  let (time, busses) = parseDataPart1 contents
      diffs = [(t, (t - time) * bus) | bus <- busses, let diff = time `mod` bus, let t = time + bus - diff]
      (_, result) = minimum diffs
  result

parseDataPart2 :: String -> [(Int, Int)]
parseDataPart2 contents = [(read bus, index) | (index, bus) <- zip [0 ..] $ splitOn "," $ last rows, bus /= "x"]
  where
    rows = lines contents

sieve' :: [(Int, Int)] -> [(Int, Int)] -> Int -> Int
sieve' _ [] result = result
sieve' active remaining result = sieve' newActive newRemaining newResult
  where
    p = product $ map fst active
    newResult =
      head
        [ n
          | n <- [result, result + p ..],
            all (\(a, b) -> (n + b) `mod` a == 0) (active ++ take 1 remaining)
        ]
    newActive = active ++ take 1 remaining
    newRemaining = tail remaining

sieve :: [(Int, Int)] -> Int
sieve busses = sieve' (take 1 busses) (tail busses) result
  where
    (result, _) = head busses

part2 :: String -> Int
part2 contents = sieve busses
  where
    busses = parseDataPart2 contents
