module AOC.Problem04 (part1, part2) where

import Data.Ix
import Data.List
import Data.List.Split

filterNot :: (a -> Bool) -> [a] -> [a]
filterNot f = filter (not . f)

containsAllFields :: [String] -> Bool
containsAllFields p = 7 == length p

birthYearValidator :: String -> Bool
birthYearValidator = inRange (1920, 2002) . (read :: String -> Int)

issueYearValidator :: String -> Bool
issueYearValidator = inRange (2010, 2020) . (read :: String -> Int)

expirationYearValidator :: String -> Bool
expirationYearValidator = inRange (2020, 2030) . (read :: String -> Int)

heightValidator :: String -> Bool
heightValidator p
  | "in" `isSuffixOf` p = inRange (59, 76) $ (read :: String -> Int) (take (length p - 2) p)
  | "cm" `isSuffixOf` p = inRange (150, 193) $ (read :: String -> Int) (take (length p - 2) p)
  | otherwise = False

hairColorValidator :: String -> Bool
hairColorValidator p
  | "#" `isPrefixOf` p = length [c | c <- p, c `elem` ['0' .. '9'] ++ ['a' .. 'f']] == 6
  | otherwise = False

eyeColorValidator :: String -> Bool
eyeColorValidator p = p `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

passportIdValidator :: String -> Bool
passportIdValidator p = length p == 9

fieldValidators :: [(String, String -> Bool)]
fieldValidators =
  [ ("byr:", birthYearValidator),
    ("iyr:", issueYearValidator),
    ("eyr:", expirationYearValidator),
    ("hgt:", heightValidator),
    ("hcl:", hairColorValidator),
    ("ecl:", eyeColorValidator),
    ("pid:", passportIdValidator)
  ]

validateField :: String -> Bool
validateField p = do
  let (Just validator) = lookup (take 4 p) fieldValidators
  validator (drop 4 p)

isPassportValid :: [String] -> Bool
isPassportValid ps = do
  let validFields = [p | p <- ps, validateField p]
  containsAllFields validFields

parseData :: String -> [[String]]
parseData contents = do
  let passports = map (concatMap words) (splitOn [""] $ lines contents)
      filteredPassports = map (filterNot (isPrefixOf "cid:")) passports
  filteredPassports

part1 :: String -> Int
part1 contents = do
  let passports = parseData contents
  length [p | p <- passports, containsAllFields p]

part2 :: String -> Int
part2 contents = do
  let passports = parseData contents
  length [p | p <- passports, containsAllFields p, isPassportValid p]