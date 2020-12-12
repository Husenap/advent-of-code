module AOC.Problem11 (part1) where

data Position
  = Floor
  | Empty
  | Occupied
  deriving (Show, Eq)

data Direction = Direction Int Int

data Coordinate = Coordinate Int Int

type Grid a = [[a]]

toPosition :: Char -> Position
toPosition 'L' = Empty
toPosition '#' = Occupied
toPosition _ = Floor

parseData :: String -> Grid Position
parseData contents = do
  let rows = lines contents
  [map toPosition r | r <- rows]

directions :: [Direction]
directions =
  [ Direction 1 0,
    Direction (-1) 0,
    Direction 0 1,
    Direction 0 (-1),
    Direction 1 1,
    Direction 1 (-1),
    Direction (-1) 1,
    Direction (-1) (-1)
  ]

neighbours :: Coordinate -> Int -> Int -> [Coordinate]
neighbours (Coordinate cr cc) numRows numCols =
  [ Coordinate r c
    | (Direction dr dc) <- directions,
      let r = cr + dr,
      let c = cc + dc,
      r >= 0,
      c >= 0,
      r < numRows,
      c < numCols
  ]

rule :: Position -> [Position] -> Position
rule Empty ns = if Occupied `notElem` ns then Occupied else Empty
rule Occupied ns = if length (filter (== Occupied) ns) >= 4 then Empty else Occupied
rule _ _ = Floor

step :: Grid Position -> Grid Position
step grid = do
  let numRows = length grid
      numCols = length (head grid)
      newGrid =
        [ [ rule
              (grid !! r !! c)
              [grid !! cr !! cc | (Coordinate cr cc) <- neighbours (Coordinate r c) numRows numCols]
            | c <- [0 .. numCols - 1]
          ]
          | r <- [0 .. numRows - 1]
        ] ::
          Grid Position
  newGrid

simulate :: Grid Position -> Grid Position
simulate grid =
  if newGrid == grid
    then newGrid
    else simulate newGrid
  where
    newGrid = step grid

calculateOccupiedSeats :: Grid Position -> Int
calculateOccupiedSeats grid = do
  let finalGrid = simulate grid
  sum [length occupied | row <- finalGrid, let occupied = filter (== Occupied) row]

part1 :: String -> Int
part1 contents = do
  let grid = parseData contents
  calculateOccupiedSeats grid