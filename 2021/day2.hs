import Data.Either
import Text.ParserCombinators.Parsec

data Instruction =
  Forward Int |
  Down Int |
  Up Int deriving Show

nums :: Parser Int
nums = do
  n <- many1 digit
  return (read n :: Int)

forward :: Parser Instruction
forward = do
  string "forward "
  Forward <$> nums

down :: Parser Instruction
down = do
  string "down "
  Down <$> nums

up :: Parser Instruction
up = do
  string "up "
  Up <$> nums

instructionParser :: Parser Instruction
instructionParser = forward <|> down <|> up

parseText :: Parser [Instruction]
parseText = sepEndBy1 instructionParser (string "\n")

parseFile = parse (parseText <* eof) "(unknown)"

task1 :: [Instruction] -> Int
task1 inst = h * d
  where
    (h, d) = foldr runInstruction (0, 0) (reverse inst)
    runInstruction (Forward x) (h, d) = (h + x, d)
    runInstruction (Down x)    (h, d) = (h,     d  + x)
    runInstruction (Up x)      (h, d) = (h,     d - x)

task2 :: [Instruction] -> Int
task2 inst = h * d
  where
    (h, d, _) = foldr runInstruction' (0, 0, 0) (reverse inst)
    runInstruction' (Forward x) (h, d, a) = (h + x, d + a * x, a)
    runInstruction' (Down x)    (h, d, a) = (h,     d,         a + x)
    runInstruction' (Up x)      (h, d, a) = (h,     d,         a - x)

main :: IO ()
main = do
  f <- readFile "input"
  let parsed = fromRight [] $ parseFile f

  putStrLn "Task1: "
  print $ task1 parsed

  putStrLn "Task2: "
  print $ task2 parsed




