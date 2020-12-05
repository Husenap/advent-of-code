import Data.List

charToBinary 'F' = 0
charToBinary 'B' = 1
charToBinary 'L' = 0
charToBinary 'R' = 1

binaryToDecimal [] = 0
binaryToDecimal (x : xs) = x + 2 * binaryToDecimal xs

neighbours (x : y : rest) = (x, y) : neighbours (y : rest)
neighbours _ = []

main = do
  contents <- readFile "input/5"

  let boardingPasses = lines contents
      binaryBoardingPasses = [map charToBinary p | p <- boardingPasses]
      parsedBoardingPasses = [row * 8 + column | p <- binaryBoardingPasses, let row = binaryToDecimal $ reverse $ take 7 p, let column = binaryToDecimal $ reverse $ drop 7 p]
      (Just surroundingSeats) = find (\(p, n) -> n - p == 2) $ neighbours $ sort parsedBoardingPasses
      seat = fst surroundingSeats + 1

  print $ "Maximum Boarding Pass Id: " ++ show (maximum parsedBoardingPasses)
  print $ "Your Boarding Pass Id: " ++ show seat
