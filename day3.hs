import Data.Either (fromRight)
import Text.ParserCombinators.Parsec
import qualified Data.Set as DS

data Direction = U Int | D Int | R Int | L Int deriving Show
-- Parsing
eol = char '\n'
parseText = sepEndBy line eol

comma = char ','
line = sepBy direction comma

direction = left <|> right <|> up <|> down

num :: Parser Int
num = do
  n <- many1 digit
  return $ read n

left = do
  char 'L'
  n <- num
  return $ L n

right = do
  char 'R'
  n <- num
  return $ R n

up = do
  char 'U'
  n <- num
  return $ U n

down = do
  char 'D'
  n <- num
  return $ D n

parseFile = parse (parseText <* eof) "(unknown)"

-- AoC challenge
dir (L n) = [((-1), 0) | _ <- [1..n]]
dir (R n) = [(1, 0)    | _ <- [1..n]]
dir (U n) = [(0, (-1)) | _ <- [1..n]]
dir (D n) = [(0, 1)    | _ <- [1..n]]

wirePath (x, y) ds = tail $ scanl fun (x, y) $ loc ds
  where
    fun (dx, dy) (x, y) = (x + dx, y + dy)
    loc ds = [(dx, dy) | d <- ds, (dx, dy) <- dir d]

distance (x, y) = abs x + abs y

sol1 (p1:p2:_) = do
  let s1 = DS.fromList p1
  let s2 = DS.fromList p2
  let i = DS.intersection s1 s2
  a <- DS.toList i
  return a

sol2 (p1:p2:_) intersections = minimum $ zipWith (+) steps1 steps2
  where
    steps1 = fmap (pathToIntersection p1) intersections
    steps2 = fmap (pathToIntersection p2) intersections

pathToIntersection path intersection = (+1) . length $ takeWhile (/= intersection) path

main :: IO ()
main = do
  f <- readFile "input.txt"
  -- f <- readFile "sample.txt"
  let parsed = fromRight [] $ parseFile f
  let paths =  (fmap (wirePath (0, 0))) parsed
  let intersections = sol1 paths
  let distances = (fmap distance) intersections
  putStrLn "Part1:"
  print $ minimum distances
  putStrLn "Part2:"
  print $ sol2 paths intersections
