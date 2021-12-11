{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Text.ParserCombinators.Parsec
import Data.Either ( fromRight )
import qualified Data.IntMap as DM

nums :: Parser Int
nums = do
  n <- many1 digit
  return (read n :: Int)

parseText :: Parser [Int]
parseText = sepBy1 nums (char ',') <* char '\n'

parseFile :: String -> Either ParseError [Int]
parseFile = parse (parseText <* eof) "(unknown)"

task fishes day = sum onTheDay
  where
    onTheDay = foldl (\fg _ -> nextGen fg) fishes' [0..day-1]
    nextGen [t0, t1, t2, t3, t4, t5, t6, t7, t8] = [t1, t2, t3, t4, t5, t6, t7 + t0, t8, t0]
    fishes' = [ length $ filter (==x) fishes | x <- [0..8]]

task1 fishes = task fishes 80
task2 fishes = task fishes 256

main :: IO ()
main = do
  f <- readFile "input"
  let seed = fromRight [] $ parseFile f

  putStrLn "Task1: "
  print $ task1 seed

  putStrLn "Task2: "
  print $ task2 seed
