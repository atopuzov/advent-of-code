import Data.Either (fromRight)
import Text.ParserCombinators.Parsec
import Control.Monad (guard)

eol = char '\n'
parseText = sepEndBy lineParser eol
lineParser = do
  char '<'
  locs <- sepBy moonParser (string ", ")
  char '>'
  return locs
signParser = do
  char '-'
  return negate
moonParser = do
  cord <- char 'x' <|> char 'y' <|> char 'z'
  char '='
  sign <- option id signParser
  n <- many1 digit
  return $ sign $ (read n :: Int)
parseFile = parse (parseText <* eof) "(unknown)"

nextLocation x y z vx vy vz = undefined
type Location = (Int, Int, Int)
type Velocity = (Int, Int, Int)
data Moon = Moon Location Velocity deriving (Show, Eq)

createMoon (x:y:z:[]) = Moon (x,y,z) (0,0,0)

calcPull (Moon location  velocity)
         (Moon location' velocity')
  = calcPull' location location'
  where
    calcPull' (x,y,z) (x',y',z') = (cp x x', cp y y', cp z z')
    cp a a' = case compare a' a of
                GT -> 1
                LT -> (-1)
                EQ -> 0

calcGravity moon moons = foldr1 comb pulls
  where
    other = filter (/=moon) moons
    pulls = fmap (calcPull moon) other
    comb (x,y,z) (x',y',z') = (x+x',y+y',z+z')

updateMoon moons moon@(Moon (x,y,z) (vx, vy, vz)) = Moon loc' vel'
  where
    (gx, gy, gz) = calcGravity moon moons
    vel'@(vx', vy',vz') = (vx + gx, vy + gy, vz + gz)
    loc'@(x',y',z') = (x + vx', y + vy', z + vz')

doTime moons = fmap (updateMoon moons) moons

energy (Moon (x,y,z) (vx, vy, vz)) = potential * kinetic
  where
    potential = abs x + abs y + abs z
    kinetic = abs vx + abs vy + abs vz

getXComponets (Moon (x,y,z) (vx, vy, vz)) = (x, vx)
getYComponets (Moon (x,y,z) (vx, vy, vz)) = (y, vy)
getZComponets (Moon (x,y,z) (vx, vy, vz)) = (z, vz)

sol2 system = foldr1 lcm [xcy, ycy, zcy]
  where
    start = head system
    extractX = fmap getXComponets
    extractY = fmap getYComponets
    extractZ = fmap getZComponets
    xComponents = fmap extractX system
    yComponents = fmap extractY system
    zComponents = fmap extractZ system
    uniqueX = takeWhile (/=(extractX start)) $ tail xComponents
    uniqueY = takeWhile (/=(extractY start)) $ tail yComponents
    uniqueZ = takeWhile (/=(extractZ start)) $ tail zComponents
    xcy = 1 + (length uniqueX)
    ycy = 1 + (length uniqueY)
    zcy = 1 + (length uniqueZ)

main :: IO ()
main = do
  f <- readFile "input.txt"
  -- f <- readFile "sample.txt"
  -- f <- readFile "sample2.txt"
  let parsed = fromRight [] $ parseFile f

  let moons = fmap createMoon parsed
  let system = iterate doTime moons
  let requested = system !! 1000
  putStrLn "Part1:"
  print $ sum $ fmap energy requested

  putStrLn "Part2:"
  print $ sol2 system
  putStrLn ""
