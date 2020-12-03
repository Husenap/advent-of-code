import qualified Data.Text as T

countNormalPasswords :: (Num a, Eq c) => [(Int, Int, c, [c])] -> a
countNormalPasswords passwords = do
  sum $
    map
      ( \p -> do
          let (min, max, char, password) = p
              filteredPassword = filter (== char) password
              filteredPasswordLength = length filteredPassword
          if filteredPasswordLength >= min && filteredPasswordLength <= max then 1 else 0
      )
      passwords

countTobogganPasswords :: (Num a, Eq c) => [(Int, Int, c, [c])] -> a
countTobogganPasswords passwords = do
  sum $
    map
      ( \p -> do
          let (min, max, char, password) = p
              firstChar = (password !! (min - 1)) == char
              secondChar = (password !! (max - 1)) == char
          if (firstChar || secondChar) && not (firstChar && secondChar) then 1 else 0
      )
      passwords

main :: IO ()
main = do
  contents <- readFile "input/2"

  let rows = lines contents
      passwords =
        map
          ( \r -> do
              let parts = words r
                  minmax = map ((read :: String -> Int) . T.unpack) (T.splitOn (T.pack "-") (T.pack (head parts)))
                  min = head minmax
                  max = minmax !! 1
                  char = head (parts !! 1)
                  password = parts !! 2
              (min, max, char, password)
          )
          rows

  print $ "normal passwords: " ++ show (countNormalPasswords passwords)
  print $ "toboggan passwords: " ++ show (countTobogganPasswords passwords)
