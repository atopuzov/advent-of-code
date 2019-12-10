import qualified Data.Map as DM
import qualified Data.List as DL

isAsteroid '#' = True
isAsteroid  _  = False

loc y x a = ((x, y), isAsteroid a)
prepLine y line = zipWith (loc y) [0..] line
readAsteroids = fmap fst . filter snd . concat . zipWith prepLine [0..]

getAngle (x, y) (x', y') = angle
  where
    angle = atan2 dx dy
    dx = fromIntegral $ x' - x
    dy = fromIntegral $ y' - y

getAngles locations location = DM.fromListWith (++) $
  fmap (\location' -> (getAngle location location', [location'])) $ filter (/=location) locations

sol1 locations = maximum $ fmap (\location -> (length . DM.keys . getAngles locations $ location, location)) locations

sol2 locations location = destroyList !! 199
  where
    destroyList = concat . DL.transpose . fmap (DL.sortOn (distance location) . snd) $ DM.toDescList angles
    angles = getAngles locations location

distance (x, y) (x', y') = abs (x - x') + abs (y - y')

main :: IO ()
main = do
  f <- readFile "input.txt"
  let asteroidLocations = readAsteroids $ lines f
  let (visibleAsteroids, bestLocation) = sol1 asteroidLocations

  putStrLn "Part1:"
  print $ visibleAsteroids

  putStrLn "Part2:"
  let (x,y) = sol2 asteroidLocations bestLocation
  print $ 100 * x + y
