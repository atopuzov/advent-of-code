{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Text.ParserCombinators.Parsec
import Data.Either
import Data.List

nums :: Parser Int
nums = do
  n <- many1 digit
  return (read n :: Int)

randomNubers = sepBy1 nums (char ',')

boardRowParser = many (char ' ') *> sepBy1 nums (many1 $ char ' ')

boardParser = sepEndBy1 boardRowParser (char '\n')

boardsParser = sepEndBy1 boardParser (string "\n")

parseText = do
  nums <- randomNubers
  spaces
  boards <- boardsParser
  return (nums, boards)

parseFile = parse (parseText <* eof) "(unknown)"

type Board = [[Int]]
type MarkedBoard = [[(Bool, Int)]]

makeMarkedBoards :: [Board] -> [MarkedBoard]
makeMarkedBoards = fmap markedBoard

markedBoard :: Board -> MarkedBoard
markedBoard = fmap (zip (repeat False))

markNumber :: Int -> MarkedBoard -> MarkedBoard
markNumber num = fmap (fmap (\(m, x) -> if x == num then (True, x) else (m, x)))

getUnmarked :: MarkedBoard -> [Int]
getUnmarked = concatMap (fmap snd . filter (\(m, x) -> not m))

hasWon :: MarkedBoard -> Bool
hasWon board = isWinning marksRows || isWinning marksCols
  where
    isWinning  = or . fmap and
    marksRows = fmap (fmap fst) board
    marksCols = transpose marksRows

task1 :: [Board] -> [Int] -> Int
task1 boards = playGame (makeMarkedBoards boards)
  where
    playGame boards (x:xs) = case winningBoard of
        [] -> playGame markedBoards xs
        [board] -> sum (getUnmarked board) * x
      where
        markedBoards = fmap (markNumber x) boards
        winningBoard = filter hasWon markedBoards

task2 :: [Board] -> [Int] -> Int
task2 boards = playGame (makeMarkedBoards boards) Nothing
  where
    playGame _ (Just (num, board)) [] = sum (getUnmarked board) * num
    playGame boards lastWinning (x:xs) = case winningBoards of
        [] -> playGame markedBoards lastWinning xs
        [board] -> playGame boardsNotWinning (Just (x, board)) xs
        bs -> playGame boardsNotWinning Nothing xs -- More then one can't be the last winning
      where
        markedBoards = fmap (markNumber x) boards
        winningBoards = filter hasWon markedBoards
        boardsNotWinning = filter (not . hasWon) markedBoards

main :: IO ()
main = do
  f <- readFile "input"
  let (numbers, boards) = fromRight ([], []) $ parseFile f

  putStrLn "Task1:"
  print $ task1 boards numbers

  putStrLn "Task2:"
  print $ task2 boards numbers


