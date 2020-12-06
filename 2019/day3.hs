import Data.Either (fromRight)
import Text.ParserCombinators.Parsec
import Data.List (scanl')
import qualified Data.Map as DM

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

wirePath (x, y) ds = DM.fromList . tail $ scanl' fun ((x, y), 0) $ loc ds
  where
    fun ((x', y'), l) (dx, dy) = ((x' + dx, y' + dy), l + 1)
    loc ds = [(dx, dy) | d <- ds, (dx, dy) <- dir d]

distance (x, y) = abs x + abs y

intersection (p1:p2:_) = DM.intersectionWith (+) p1 p2

sol1 = minimum . fmap distance . DM.keys
sol2 = minimum . DM.elems

main :: IO ()
main = do
  f <- readFile "input.txt"
  -- f <- readFile "sample.txt"
  let parsed = fromRight [] $ parseFile f
  let paths =  (fmap (wirePath (0, 0))) parsed
  let intersections = intersection paths
  putStrLn "Part1:"
  print $ sol1 intersections
  putStrLn "Part2:"
  print $ sol2 intersections
