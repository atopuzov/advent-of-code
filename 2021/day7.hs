import Text.ParserCombinators.Parsec
import Data.Either ( fromRight )
import Data.List (sort)

nums :: Parser Int
nums = do
  n <- many1 digit
  return (read n :: Int)

parseText :: Parser [Int]
parseText = sepBy1 nums (char ',') <* char '\n'

parseFile :: String -> Either ParseError [Int]
parseFile = parse (parseText <* eof) "(unknown)"

task cost locations = minimum costs
  where
    allLocations = [minimum locations .. maximum locations]
    costs = fmap costForLocation allLocations
    costForLocation p = sum $ fmap (cost . distance p) locations
    distance x y = abs(x - y)

task1 :: [Int] -> Int
task1 = task id

task2 :: [Int] -> Int
task2 = task cost
  where
    cost n = n * (n + 1) `div` 2


main :: IO ()
main = do
  f <- readFile "input"
  let seed = fromRight [] $ parseFile f

  putStrLn "Task1: "
  print $ task1 seed

  putStrLn "Task2: "
  print $ task2 seed
