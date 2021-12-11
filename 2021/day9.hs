import Text.ParserCombinators.Parsec
import Data.Either ( fromRight )
import qualified Data.Map as DM
import qualified Data.Set as DS
import Data.Maybe (catMaybes)
import Data.List (sort)

num :: Parser Int
num = do
  n <- digit
  return (read [n] :: Int)

parseText :: Parser [[Int]]
parseText = sepEndBy1 (many1 num) (char '\n')

parseFile :: String -> Either ParseError [[Int]]
parseFile = parse (parseText <* eof) "(unknown)"

findLowPoints dm = lowPoints
  where
    lowPoints = DM.keys $ DM.filter id $ DM.mapWithKey isLow dm
    isLow loc@(x, y) v = all (>v) mloc
      where
        mloc = catMaybes $ (`DM.lookup` dm) <$> considered
        considered = [(x-1, y), (x+1, y), (x, y+1), (x, y-1)]

processInput = DM.fromList . concat . zipWith (\x -> zipWith (\y v -> ((x, y), v)) [0..] ) [0..]

task1 nums = sum $ fmap (\x -> nums DM.! x + 1) lowPoints
  where
    lowPoints = findLowPoints nums

task2 nums = product largest3
  where
    largest3 = take 3 $ reverse $ sort basinSizes
    basinSizes = fmap (DS.size . findBasin) lowPoints
    lowPoints = findLowPoints nums
    findNeighbours (x, y) = filter (`DM.member` nums) [(x+1, y), (x-1, y), (x, y-1), (x, y+1)]

    findBasin loc = go DS.empty loc
      where
        go seen loc =
          if nums DM.! loc == 9 || loc `DS.member` seen
          then seen
          else seen''
          where
            seen' = DS.insert loc seen
            seen'' = foldl go seen' $ findNeighbours loc

main :: IO ()
main = do
  f <- readFile "input"
  let i = fromRight [] $ parseFile f
  let dm = processInput i

  putStrLn "Task1:"
  print $ task1 dm

  putStrLn "Task2:"
  print $ task2 dm
