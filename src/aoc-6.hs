import Data.List
import Data.List.Split

getTotalNumberOfAnswers = sum . map (length . foldl union [])

getNumberOfGroupAnswers = sum . map (length . foldl intersect ['a' .. 'z'])

main = do
  contents <- readFile "input/6"

  let groups = splitOn [""] (lines contents)

  putStrLn $ "Total number of answers: " ++ show (getTotalNumberOfAnswers groups)
  putStrLn $ "Total number of group answers: " ++ show (getNumberOfGroupAnswers groups)
