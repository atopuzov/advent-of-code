import qualified Data.Map as DM
import Data.Maybe (mapMaybe)

bounds dm = ([xmin..xmax], [ymin..ymax])
  where
    k = DM.keys dm
    xs = fmap fst k
    ys = fmap snd k
    xmin = minimum xs
    xmax = maximum xs
    ymin = minimum ys
    ymax = maximum ys

printImage dm = unlines image
  where    
    (xrange, yrange) = bounds dm
    image = [ [ dm DM.! (x, y) | x <- xrange ] | y <- yrange ]

loc y x a = ((x, y), a)
prepLine y line = zipWith (loc y) [0..] line
readImage = concat . zipWith prepLine [0..]

emptySeat = 'L'
occupiedSeat = '#'

isSeat x = x == occupiedSeat || x == emptySeat
isOccupiedSeat x = x == occupiedSeat
isEmpySeat x = x == emptySeat

seatsAround dm (x, y) = mapMaybe (`DM.lookup` dm) around
    where
        around = [ (x + i, y + j) | i <- [-1,0,1], j <- [-1,0,1], (i, j) /= (0,0)]

ray dm (x, y) d@(dx, dy) = 
    DM.lookup nextLoc dm >>= f
        where
            f x | isSeat x  = Just x
                | otherwise = ray dm nextLoc d
            nextLoc = (x + dx, y + dy)

seatsVisible dm loc =  mapMaybe (ray dm loc) dirs
    where
        dirs = [(i, j) | i <- [-1,0,1], j <- [-1,0,1], (i, j) /= (0,0)]

firstRepeating (x:y:r)
    | x == y = x
    | otherwise = firstRepeating (y:r)

numberOfOccupied = length . filter isOccupiedSeat

step findSeats occupancy dm = DM.mapWithKey step' dm
    where
        step' loc val = f val
            where
                f x | isEmpySeat     x && occupied == 0         = occupiedSeat
                    | isOccupiedSeat x && occupied >= occupancy = emptySeat
                    | otherwise = x
                around = findSeats dm loc
                occupied = numberOfOccupied around

step1 = step seatsAround 4
step2 = step seatsVisible 5


sol1 = sol step1
sol2 = sol step2

sol step floor = occupied
    where
        all = iterate step floor  
        repeating = firstRepeating all
        occupied = numberOfOccupied $ DM.elems repeating

main :: IO ()
main = do
    -- f <- readFile "sample"
    -- f <- readFile "sample2"
    -- f <- readFile "sample3"
    -- f <- readFile "sample4"
    f <- readFile "input"

    let floor =  DM.fromList $ readImage $ lines f

    putStr "Task1: "
    print $ sol1 floor

    putStr "Task2: "
    print $ sol2 floor
