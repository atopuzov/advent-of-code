import Text.ParserCombinators.Parsec
import qualified Data.Map as DM
import qualified Data.Set as DS
import Data.Either ( fromRight )
import Data.Maybe
import Data.Char ( intToDigit )

num :: Parser Int
num = do
  n <- digit
  return (read [n] :: Int)

type Pos = (Int, Int)

parseText = do
  sepEndBy1 (many1 num) (char '\n')

parseFile = parse (parseText <* eof) "(unknown)"

processInput :: [[Int]] -> DM.Map Pos Int
processInput = DM.fromList . concat . zipWith (\y -> zipWith (\x v -> ((x, y), v)) [0..] ) [0..]

processInput2 :: [[Int]] -> DM.Map Pos Int
processInput2 = processInput . concat . take 5 . iterate (map increase) . fmap row
  where
    row = concat . take 5 . iterate increase
    increase = map $ \x -> (x `mod` 9) + 1

spfa graph start target = loop DS.empty (DS.singleton (0, start))
  where
    costAndLoc cost loc = zip values locs
      where
        values = fmap (\x -> cost + graph DM.! x) locs
        locs = next loc
    next (x, y) = filter (`DM.member` graph) [(x+1, y), (x-1, y), (x, y-1), (x, y+1)]

    loop visited toBeVisited =
      case DS.minView toBeVisited of
        Nothing -> Nothing
        Just ((cost, node), withoutNode)
          | node == target           -> Just (cost, node)
          | node `DS.member` visited -> loop visited withoutNode
          | otherwise                -> loop visitedWithNode withNext
          where
            visitedWithNode = DS.insert node visited
            withNext = foldr DS.insert withoutNode $  costAndLoc cost node

solve graph = fst <$> spfa graph start target
  where
    start = (0, 0)
    positions = DM.keys graph
    target = (maximum $ fmap fst positions, maximum $ fmap snd positions)

task1 :: [[Int]] -> Maybe Int
task1 = solve . processInput

task2 :: [[Int]] -> Maybe Int
task2 = solve . processInput2

main :: IO ()
main = do
  f <- readFile "input"
  let i = fromRight [] $ parseFile f

  putStrLn "Task1:"
  print $ task1 i

  putStrLn "Task2:"
  print $ task2 i

