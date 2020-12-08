{-# LANGUAGE RecordWildCards #-}

module AOC.Problem02 where

import qualified Data.Text as T

data PasswordRule = PasswordRule
  { min' :: Int,
    max' :: Int,
    char :: Char,
    password :: String
  }

countNormalPasswords :: [PasswordRule] -> Int
countNormalPasswords passwords = do
  sum $
    map
      ( \PasswordRule {..} -> do
          let filteredPassword = filter (== char) password
              filteredPasswordLength = length filteredPassword
          if filteredPasswordLength >= min' && filteredPasswordLength <= max' then 1 else 0
      )
      passwords

countTobogganPasswords :: [PasswordRule] -> Int
countTobogganPasswords passwords = do
  sum $
    map
      ( \PasswordRule {..} -> do
          let firstChar = (password !! (min' - 1)) == char
              secondChar = (password !! (max' - 1)) == char
          if (firstChar || secondChar) && not (firstChar && secondChar) then 1 else 0
      )
      passwords

parseData :: String -> [PasswordRule]
parseData contents = do
  let rows = lines contents
      passwords =
        map
          ( \r -> do
              let parts = words r
                  minmax = map ((read :: String -> Int) . T.unpack) (T.splitOn (T.pack "-") (T.pack (head parts)))
              PasswordRule
                { min' = head minmax,
                  max' = minmax !! 1,
                  char = head (parts !! 1),
                  password = parts !! 2
                }
          )
          rows
  passwords

part1 :: String -> Int
part1 contents = countNormalPasswords $ parseData contents

part2 :: String -> Int
part2 contents = countTobogganPasswords $ parseData contents
