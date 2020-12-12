module AOC.Problem12.Part2 (part2) where

import AOC.Problem12.Shared

data Location = Location Point Point

baseLocation :: Location
baseLocation = Location basePoint (Point 10 1)

rotateRight :: Point -> Point
rotateRight (Point x y) = Point y (negate x)

rotateLeft :: Point -> Point
rotateLeft (Point x y) = Point (negate y) x

apply :: Location -> Instruction -> Location
apply (Location p (Point wx wy)) (Instruction N v) = Location p (Point wx (wy + v))
apply (Location p (Point wx wy)) (Instruction S v) = Location p (Point wx (wy - v))
apply (Location p (Point wx wy)) (Instruction E v) = Location p (Point (wx + v) wy)
apply (Location p (Point wx wy)) (Instruction W v) = Location p (Point (wx - v) wy)
apply (Location p w) (Instruction R v) = Location p (iterate rotateRight w !! (v `div` 90))
apply (Location p w) (Instruction L v) = Location p (iterate rotateLeft w !! (v `div` 90))
apply (Location (Point x y) (Point wx wy)) (Instruction F v) = Location (Point (x + wx * v) (y + wy * v)) (Point wx wy)

simulate :: Location -> [Instruction] -> Location
simulate location [] = location
simulate location instructions = do
  let newLocation = apply location (head instructions)
  simulate newLocation (tail instructions)

part2 :: String -> Int
part2 contents = do
  let instructions = parseData contents
      (Location finalPoint _) = simulate baseLocation instructions
  manhattanDistance finalPoint
