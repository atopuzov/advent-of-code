import qualified Data.Map as DM
import Data.Maybe (mapMaybe)


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


main :: IO ()
main = do
    -- f <- readFile "sample"
    f <- readFile "input"

    let floor =  DM.fromList $ readImage $ lines f

    putStrLn "Task1: "
    let all = iterate step1 floor  
    let repeating = firstRepeating all
    let occupied = numberOfOccupied $ DM.elems repeating
    print $ occupied

    putStr "Task2: "
    let all2 = iterate step2 floor
    let repeating2 = firstRepeating all2
    let occupied2 = numberOfOccupied $ DM.elems repeating2
    print $ occupied2
