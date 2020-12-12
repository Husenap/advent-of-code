module AOC.Problem07 (part1, part2) where

import Data.List
import Data.List.Split
import Data.Maybe

calculateNumberOfBags :: [(String, [(Int, String)])] -> String -> Int
calculateNumberOfBags definitions current = case lookup current definitions of
  Just bag -> sum [n + n * calculateNumberOfBags definitions c | (n, c) <- bag]
  Nothing -> 0

canContainShinyGoldBag :: [(String, [String])] -> (String, [String]) -> Bool
canContainShinyGoldBag definitions bag =
  "shiny gold" `elem` snd bag
    || or
      [ canContainShinyGoldBag definitions (fromJust d)
        | c <- snd bag,
          let d = find (\b -> fst b == c) definitions,
          isJust d
      ]

parseData :: String -> [[String]]
parseData contents = do
  let lines' = [take (length l - 1) l | l <- lines contents] -- drop period (.) at the end of the line
      splitLines = [splitOn " bags contain " b | b <- lines'] -- separate bag color and what it can contain
  splitLines

part1 :: String -> Int
part1 contents = do
  let splitLines = parseData contents
      definitions =
        [ ( c,
            map (unwords . tail . init . words) (splitOn ", " o)
          )
          | c : o : _ <- splitLines,
            o /= "no other bags"
        ]
      candidates = filter (canContainShinyGoldBag definitions) definitions
  length candidates

part2 :: String -> Int
part2 contents = do
  let splitLines = parseData contents
      definitions =
        [ ( c,
            map ((\(n : r) -> ((read :: String -> Int) n, unwords $ init r)) . words) (splitOn ", " o)
          )
          | c : o : _ <- splitLines,
            o /= "no other bags"
        ]
  calculateNumberOfBags definitions "shiny gold"