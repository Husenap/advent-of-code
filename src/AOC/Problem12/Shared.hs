module AOC.Problem12.Shared where

data Point = Point Int Int deriving (Show)

basePoint :: Point
basePoint = Point 0 0

manhattanDistance :: Point -> Int
manhattanDistance (Point x y) = abs x + abs y

data Action = N | S | E | W | L | R | F deriving (Show)

toAction :: Char -> Action
toAction 'N' = N
toAction 'S' = S
toAction 'E' = E
toAction 'W' = W
toAction 'L' = L
toAction 'R' = R
toAction 'F' = F
toAction _ = F

data Instruction = Instruction Action Int deriving (Show)

parseData :: String -> [Instruction]
parseData contents = [Instruction (toAction action) (read value) | (action : value) <- lines contents]