module AOC.Problem08 where

import Data.Maybe

parseData :: String -> [(String, Int)]
parseData contents =
  [ (i, (read :: String -> Int) n)
    | l <- lines (filter (/= '+') contents),
      let i : n : _ = words l
  ]

evaluatePart1' :: [(String, Int)] -> [Int] -> Int -> Int
evaluatePart1' instructions visited instructionPointer = do
  let (instruction, value) = instructions !! instructionPointer
  let newInstructionPointer = instructionPointer + (if instruction == "jmp" then value else 1)
      newVisited = newInstructionPointer : visited

  if newInstructionPointer `elem` visited
    then 0
    else case instruction of
      "nop" -> evaluatePart1' instructions newVisited newInstructionPointer
      "acc" -> value + evaluatePart1' instructions newVisited newInstructionPointer
      "jmp" -> evaluatePart1' instructions newVisited newInstructionPointer
      _ -> 0

evaluatePart1 :: [(String, Int)] -> Int
evaluatePart1 instructions = evaluatePart1' instructions [] 0

evaluatePart2' :: [(String, Int)] -> [Int] -> Int -> Bool -> Maybe Int
evaluatePart2' instructions visited instructionPointer hasChangedAnInstruction
  | instructionPointer `elem` visited = Nothing
  | instructionPointer == length instructions = Just 0
  | instructionPointer > length instructions = Nothing
  | otherwise = do
    let (instruction, value) = instructions !! instructionPointer
        newVisited = instructionPointer : visited
    case instruction of
      "acc" -> case evaluatePart2' instructions newVisited (instructionPointer + 1) hasChangedAnInstruction of
        Just result -> Just (value + result)
        Nothing -> Nothing
      "nop" -> case evaluatePart2' instructions newVisited (instructionPointer + 1) hasChangedAnInstruction of
        Just result -> Just result
        Nothing -> if hasChangedAnInstruction then Nothing else evaluatePart2' instructions newVisited (instructionPointer + value) True
      "jmp" -> case evaluatePart2' instructions newVisited (instructionPointer + value) hasChangedAnInstruction of
        Just result -> Just result
        Nothing -> if hasChangedAnInstruction then Nothing else evaluatePart2' instructions newVisited (instructionPointer + 1) True
      _ -> Nothing

evaluatePart2 :: [(String, Int)] -> Int
evaluatePart2 instructions = fromMaybe 0 $ evaluatePart2' instructions [] 0 False

part1 :: String -> Int
part1 contents = do
  let instructions = parseData contents
  evaluatePart1 instructions

part2 :: String -> Int
part2 contents = do
  let instructions = parseData contents
  evaluatePart2 instructions