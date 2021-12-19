import Data.Either (fromRight)
import qualified Data.Map as DM
import qualified Data.Set as DS
import Text.ParserCombinators.Parsec

nums :: Parser Int
nums = do
  n <- many1 digit
  return (read n :: Int)

type FMap = DM.Map String Char

type Input = (String, FMap)

formula :: Parser (String, Char)
formula = do
  s <- many1 letter
  string " -> "
  e <- letter
  return (s, e)

parseText :: Parser (String, FMap)
parseText = do
  template <- many1 letter
  spaces
  rules <- sepEndBy1 formula (char '\n')
  return (template, DM.fromList rules)

parseFile :: String -> Either ParseError Input
parseFile = parse (parseText <* eof) "(unknown)"

task :: Int -> String -> FMap -> Int
task atStep template rules = maxC - minC
  where
    -- Add first and last
    totalCount = DM.unionWith (+) (DM.singleton (head template) 1) countAtStep
    countAtStep = doCount $ iterate step initialCounts !! atStep
    minC = minimum $ DM.elems totalCount
    maxC = maximum $ DM.elems totalCount

    pairs lst = zipWith (\x y -> [x, y]) lst (tail lst)
    initialCounts = DM.fromListWith (+) $ zip (pairs template) (repeat 1)

    produce [a, b] n = DM.fromListWith (+) [([a, i], n), ([i, b], n)]
      where
        i = rules DM.! [a, b]
    produce _ _ = DM.empty

    step freqs = DM.foldrWithKey ((DM.unionWith (+) .) . produce) DM.empty freqs

    doCount = DM.foldrWithKey ((DM.unionWith (+) .) . myCount) DM.empty
    myCount [a, b] x = DM.singleton b x
    myCount _ _ = DM.empty

task1 :: String -> FMap -> Int
task1 = task 10

task2 :: String -> FMap -> Int
task2 = task 40

main :: IO ()
main = do
  f <- readFile "input"
  let (template, rules) = fromRight ([], DM.empty) $ parseFile f

  putStrLn "Task1:"
  print $ task1 template rules

  putStrLn "Task2:"
  print $ task2 template rules
