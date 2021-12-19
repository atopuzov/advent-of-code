import Text.ParserCombinators.Parsec
import Data.Either ( fromRight )
import Data.Maybe
import Data.List
import qualified Data.Ord

nums :: Parser Int
nums = do
  sign <- option id signParser
  n <- many1 digit
  return $ sign (read n :: Int)
  where
    signParser = do
    char '-'
    return negate

parseRange :: Char -> Parser (Int, Int)
parseRange c = do
  char c
  char '='
  startR <- nums
  string ".."
  endR <- nums
  pure (startR, endR)

parseText :: Parser ((Int, Int),(Int, Int))
parseText = do
  string "target area: "
  x <- parseRange 'x'
  string ", "
  y <- parseRange 'y'
  pure (x, y)

parseFile :: String -> Either ParseError ((Int, Int),(Int, Int))
parseFile = parse (parseText <* spaces <* eof) "(unknown)"

solve :: ((Int,Int), (Int,Int)) -> [(Int, (Int, Int))]
solve ((xMin, xMax), (yMin, yMax)) = succesffullVs
  where
    minVX = 1;            maxVX = xMax
    minVY = - (abs yMin); maxVY = abs yMin
    possibleVs = [(x,y) | x <- [minVX..maxVX],  y <- [minVY..maxVY]]
    succesffullVs = mapMaybe myFunc possibleVs
    myFunc v =
            if inTargetArea lastLoc
            then Just (maxY, v)
            else Nothing
        where
          maxY = maximum $ fmap snd trajectory
          lastLoc = last trajectory
          trajectory = fst <$> takeWhile doMore positions
          doMore ((x,y),_) = x >=0 && x <= xMax && y >= yMin
          positions = iterate step ((0, 0), v)
    inTargetArea (x, y) = x >= xMin && x <= xMax && y >= yMin && y <= yMax

    step ((x,y), (xV, yV)) = ((x',y'), (xV', yV'))
      where
        x' = x + xV
        y' = y + yV
        xV' | xV > 0 = xV - 1
            | xV < 0 = xV + 1
            | otherwise = 0
        yV' = yV - 1

task1 :: ((Int, Int), (Int, Int)) -> Int
task1 targetArea = fst $ head sortedVs
  where
    sortedVs = sortOn (Data.Ord.Down . fst) succesffullVs
    succesffullVs = solve targetArea

task2 :: ((Int, Int), (Int, Int)) -> Int
task2 = length . solve

main :: IO ()
main = do
  f <- readFile "input"
  let targetArea = fromRight ((0,0),(0,0)) $ parseFile f

  putStrLn "Task1:"
  print $ task1 targetArea

  putStrLn "Task2:"
  print $ task2 targetArea
