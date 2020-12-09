module AOC.Problem09 where

import Data.List

parseData :: String -> [Int]
parseData = map (read :: String -> Int) . lines

calculateSums :: [Int] -> [Int]
calculateSums numbers = [n + m | (n : ns) <- tails numbers, m <- ns]

findNumberThatBreaksRule :: [Int] -> Int -> Int
findNumberThatBreaksRule numbers preambleSize = do
  let preambleSums = calculateSums $ take preambleSize numbers
      number = numbers !! preambleSize
  if number `elem` preambleSums
    then findNumberThatBreaksRule (tail numbers) preambleSize
    else number

part1 :: String -> Int -> Int
part1 contents preambleSize = do
  let numbers = parseData contents
  findNumberThatBreaksRule numbers preambleSize

findEncryptionWeakness :: [Int] -> Int -> Int
findEncryptionWeakness numbers invalidNumber = do
  let solution =
        take
          1
          [ maximum g + minimum g
            | s <- [2 .. length numbers],
              ns <- tails numbers,
              let g = take s ns,
              length g == s,
              sum g == invalidNumber
          ]
  if null solution
    then 0
    else head solution

part2 :: String -> Int -> Int
part2 contents preambleSize = do
  let numbers = parseData contents
      invalidNumber = findNumberThatBreaksRule numbers preambleSize
  findEncryptionWeakness numbers invalidNumber