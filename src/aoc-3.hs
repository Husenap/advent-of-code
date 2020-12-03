countTreeHits dx dy treeMap = do
  let pairs = zip [0 ..] treeMap
      filteredPairs = filter (\(i, _) -> mod i dy == 0) pairs
      filteredMap = map snd filteredPairs
      indexedMap = zip [0 ..] filteredMap
      hitMap =
        map
          ( \(i, c) -> do
              if c !! mod (i * dx) (length c) == '#' then 1 else 0
          )
          indexedMap

  sum hitMap

main = do
  contents <- readFile "input/3"

  let treeMap = lines contents
      treeHits11 = countTreeHits 1 1 treeMap
      treeHits31 = countTreeHits 3 1 treeMap
      treeHits51 = countTreeHits 5 1 treeMap
      treeHits71 = countTreeHits 7 1 treeMap
      treeHits12 = countTreeHits 1 2 treeMap
      product = treeHits11 * treeHits31 * treeHits51 * treeHits71 * treeHits12

  print $ "slope 1 1:" ++ show treeHits11
  print $ "slope 3 1:" ++ show treeHits31
  print $ "slope 5 1:" ++ show treeHits51
  print $ "slope 7 1:" ++ show treeHits71
  print $ "slope 1 2:" ++ show treeHits12

  print $ "product of slopes: " ++ show product

  return ()