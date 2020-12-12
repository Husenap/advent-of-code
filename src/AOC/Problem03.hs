module AOC.Problem03 (part1, part2) where

countTreeHits :: Int -> Int -> [String] -> Int
countTreeHits dx dy treeMap = do
  let pairs = zip [0 ..] treeMap
      filteredPairs = filter (\(i, _) -> mod i dy == 0) pairs
      indexedMap = zip [0 ..] (map snd filteredPairs)
      hitMap =
        map
          ( \(i, c) -> do
              if c !! mod (i * dx) (length c) == '#' then 1 else 0
          )
          indexedMap
  sum hitMap

part1 :: String -> Int
part1 contents = do
  let treeMap = lines contents
  countTreeHits 3 1 treeMap

part2 :: String -> Int
part2 contents = do
  let treeMap = lines contents
  product [countTreeHits x y treeMap | (x, y) <- [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]]
