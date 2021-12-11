import Text.ParserCombinators.Parsec
import Data.Either
import qualified Data.Map as DM
import Data.List
import Debug.Trace

nums :: Parser Int
nums = do
  n <- many1 digit
  return (read n :: Int)

type Point = (Int, Int)
type LineDef = (Point, Point)

lineParser :: Parser LineDef
lineParser = do
  x1 <- nums
  char ','
  y1 <- nums
  string " -> "
  x2 <- nums
  char ','
  y2 <- nums
  return ((x1, y1), (x2, y2))

parseText = sepEndBy1 lineParser (char '\n')
parseFile = parse (parseText <* eof) "(unknown)"

isHorOrVert :: LineDef -> Bool
isHorOrVert ((x1, y1), (x2, y2)) = x1 == x2 || y1 == y2

linePoints :: LineDef -> [Point]
linePoints line@((x1, y1), (x2, y2)) = a
  where
    a = [(x1 + t*dx , y1 + t*dy) | t <- [0..l]]
    l = max (abs (x1 - x2)) (abs (y1 - y2))
    dx = signum (x2 - x1)
    dy = signum (y2 - y1)

task :: [LineDef] -> Int
task lins = solution
  where
    solution = length $ filter (>=2) $ fmap length $ group $ sort points
    points = concatMap linePoints lins

task1 :: [LineDef] -> Int
task1 lins = task $ filter isHorOrVert lins

task2 :: [LineDef] -> Int
task2 = task

main :: IO ()
main = do
  f <- readFile "input"
  let lins = fromRight [] $ parseFile f

  putStrLn "Task1: "
  print $ task1 lins

  putStrLn "Task2: "
  print $ task2 lins
