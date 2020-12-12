module AOC.Problem05 (part1, part2) where

import Data.List

charToBinary :: Char -> Int
charToBinary 'F' = 0
charToBinary 'B' = 1
charToBinary 'L' = 0
charToBinary 'R' = 1
charToBinary _ = 0

binaryToDecimal :: [Int] -> Int
binaryToDecimal [] = 0
binaryToDecimal (x : xs) = x + 2 * binaryToDecimal xs

neighbours :: [a] -> [(a, a)]
neighbours (x : y : rest) = (x, y) : neighbours (y : rest)
neighbours _ = []

parseData :: String -> [Int]
parseData contents = do
  let binaryBoardingPasses = [map charToBinary p | p <- lines contents]
      parsedBoardingPasses =
        [ row * 8 + column
          | p <- binaryBoardingPasses,
            let row = binaryToDecimal $ reverse $ take 7 p,
            let column = binaryToDecimal $ reverse $ drop 7 p
        ]
  parsedBoardingPasses

part1 :: String -> Int
part1 contents = do
  let passes = parseData contents
  maximum passes

part2 :: String -> Int
part2 contents = do
  let passes = parseData contents
      (Just surroundingSeats) = find (\(p, n) -> n - p == 2) $ neighbours $ sort passes
      seat = fst surroundingSeats + 1
  seat
