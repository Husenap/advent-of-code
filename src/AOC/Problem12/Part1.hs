module AOC.Problem12.Part1 (part1) where

import AOC.Problem12.Shared

data Location = Location Point Action deriving (Show)

baseLocation :: Location
baseLocation = Location basePoint E

rotateRight :: Action -> Action
rotateRight N = E
rotateRight E = S
rotateRight S = W
rotateRight W = N
rotateRight a = a

rotateLeft :: Action -> Action
rotateLeft N = W
rotateLeft W = S
rotateLeft S = E
rotateLeft E = N
rotateLeft a = a

apply :: Location -> Instruction -> Location
apply (Location (Point x y) d) (Instruction N v) = Location (Point x (y + v)) d
apply (Location (Point x y) d) (Instruction S v) = Location (Point x (y - v)) d
apply (Location (Point x y) d) (Instruction E v) = Location (Point (x + v) y) d
apply (Location (Point x y) d) (Instruction W v) = Location (Point (x - v) y) d
apply (Location (Point x y) d) (Instruction L v) = Location (Point x y) (iterate rotateLeft d !! (v `div` 90))
apply (Location (Point x y) d) (Instruction R v) = Location (Point x y) (iterate rotateRight d !! (v `div` 90))
apply (Location (Point x y) d) (Instruction F v) = apply (Location (Point x y) d) (Instruction d v)

simulate :: Location -> [Instruction] -> Location
simulate location [] = location
simulate location instructions = do
  let newLocation = apply location (head instructions)
  simulate newLocation (tail instructions)

part1 :: String -> Int
part1 contents = do
  let instructions = parseData contents
      (Location finalPoint _) = simulate baseLocation instructions
  manhattanDistance finalPoint