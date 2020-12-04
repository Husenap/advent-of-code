import Data.List
import Data.List.Split

filterNot f = filter (not . f)

containsAllFields p = 7 == length p

birthYearValidator p = do
  let year = read p
  year <= 2002 && year >= 1920

issueYearValidator p = do
  let year = read p
  year <= 2020 && year >= 2010

expirationYearValidator p = do
  let year = read p
  year <= 2030 && year >= 2020

heightValidator p
  | "in" `isSuffixOf` p = do
    let inches = read (take (length p - 2) p)
    inches <= 76 && inches >= 59
  | "cm" `isSuffixOf` p = do
    let centimeters = read (take (length p - 2) p)
    centimeters <= 193 && centimeters >= 150
  | otherwise = False

hairColorValidator p
  | "#" `isPrefixOf` p = length [c | c <- p, c `elem` ['0' .. '9'] ++ ['a' .. 'f']] == 6
  | otherwise = False

eyeColorValidator p = p `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

passportIdValidator p = length p == 9

fieldValidators =
  [ ("byr:", birthYearValidator),
    ("iyr:", issueYearValidator),
    ("eyr:", expirationYearValidator),
    ("hgt:", heightValidator),
    ("hcl:", hairColorValidator),
    ("ecl:", eyeColorValidator),
    ("pid:", passportIdValidator)
  ]

validateField p = do
  let (Just validator) = lookup (take 4 p) fieldValidators
  validator (drop 4 p)

isPassportValid ps = do
  let validFields = [p | p <- ps, validateField p]
  containsAllFields validFields

main = do
  contents <- readFile "input/4"
  let passports = map (concatMap words) (splitOn [""] $ lines contents)
      filteredPassports = map (filterNot (isPrefixOf "cid:")) passports
      passportsWithCorrectFields = [p | p <- filteredPassports, containsAllFields p]
      validatedPassports = [p | p <- passportsWithCorrectFields, isPassportValid p]

  print $ "Number of Passports with Correct Fields: " ++ show (length passportsWithCorrectFields)
  print $ "Number of Valid Passports: " ++ show (length validatedPassports)
