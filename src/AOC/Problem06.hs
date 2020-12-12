module AOC.Problem06 (part1, part2) where

import Data.List
import Data.List.Split

getTotalNumberOfAnswers :: [[String]] -> Int
getTotalNumberOfAnswers = sum . map (length . foldl union [])

getNumberOfGroupAnswers :: [[String]] -> Int
getNumberOfGroupAnswers = sum . map (length . foldl intersect ['a' .. 'z'])

part1 :: String -> Int
part1 contents = getTotalNumberOfAnswers $ splitOn [""] (lines contents)

part2 :: String -> Int
part2 contents = getNumberOfGroupAnswers $ splitOn [""] (lines contents)