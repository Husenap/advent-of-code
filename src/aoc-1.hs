import Data.List

pairs l = [(x + y, x * y) | (x : ys) <- tails l, y <- ys]

triplets l = [(x + y + z, x * y * z) | (x : ys) <- tails l, (y : zs) <- tails ys, z <- zs]

main = do
  contents <- readFile "input/1"
  let numbers = map (read :: String -> Int) (lines contents)

  print $ find (\a -> fst a == 2020) (pairs numbers)
  print $ find (\a -> fst a == 2020) (triplets numbers)

  return ()
