{-# LANGUAGE TupleSections #-}

import Data.Either (fromRight)
import qualified Data.Set as DS
import Text.ParserCombinators.Parsec

nums :: Parser Int
nums = do
  n <- many1 digit
  return (read n :: Int)

type Point = (Int, Int)

type Paper = DS.Set Point

type Instruction = (Char, Int)

type Instructions = [Instruction]

parsePoint :: Parser Point
parsePoint = do
  x <- nums
  char ','
  y <- nums
  return (x, y)

parsePoints :: Parser [Point]
parsePoints = sepEndBy1 parsePoint (char '\n')

parseFoldInstruction :: Parser Instruction
parseFoldInstruction = do
  string "fold along "
  c <- oneOf "xy"
  char '='
  n <- nums
  return (c, n)

parseFoldInstructions :: Parser Instructions
parseFoldInstructions = sepEndBy1 parseFoldInstruction (char '\n')

parseText :: Parser (Paper, Instructions)
parseText = do
  points <- parsePoints
  spaces
  instructions <- parseFoldInstructions
  return (DS.fromList points, instructions)

parseFile :: String -> Either ParseError (Paper, Instructions)
parseFile = parse (parseText <* eof) "(unknown)"

foldPaper :: Paper -> Instruction -> Paper
foldPaper paper (c, l)
  | c == 'y' = DS.map ( \(x, y) -> (x              , l - abs (l -y)) ) paper
  | c == 'x' = DS.map ( \(x, y) -> (l - abs (l - x), y) ) paper
  | otherwise = error "Unknown fold!"

task1 :: Paper -> [Instruction] -> Int
task1 dm [] = error "Need instructions!"
task1 dm (f : _) = DS.size $ foldPaper dm f

task2 :: Paper -> [Instruction] -> Paper
task2 = foldl foldPaper

showPaper :: Paper -> String
showPaper paper = unlines [row y | y <- [0 .. maxY]]
  where
    maxX = maximum $ fst <$> DS.toList paper
    maxY = maximum $ snd <$> DS.toList paper
    row y = [p (x, y) | x <- [0 .. maxX]]
    p (x, y) = if DS.member (x, y) paper then '█' else ' '

main :: IO ()
main = do
  f <- readFile "input"
  let (points, folds) = fromRight (DS.empty, []) $ parseFile f

  putStrLn "Task1:"
  print $ task1 points folds

  putStrLn "Task2:"
  putStr $ showPaper $ task2 points folds
