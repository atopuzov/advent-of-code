import qualified Data.Map as DM
import Data.Maybe (mapMaybe, fromMaybe)

bounds image = (xmin, xmax, ymin, ymax, zmin, zmax, wmin, wmax)
  where
    k = DM.keys $ DM.filter id image
    xs = fmap (\(x,_,_,_) -> x) k
    ys = fmap (\(_,y,_,_) -> y) k
    zs = fmap (\(_,_,z,_) -> z) k
    ws = fmap (\(_,_,_,w) -> w) k
    (xmin, xmax) = (minimum xs, maximum xs)
    (ymin, ymax) = (minimum ys, maximum ys)
    (zmin, zmax) = (minimum zs, maximum zs)
    (wmin, wmax) = (minimum ws, maximum ws)

extendBounds image = ([xmin - 1 .. xmax + 1], 
                       [ymin - 1 .. ymax + 1], 
                       [zmin - 1 .. zmax + 1],
                       [wmin - 1 .. wmax + 1])
  where
    (xmin, xmax, ymin, ymax, zmin, zmax, wmin, wmax) = bounds image 

type Coordinate = Int
type Location4d = (Coordinate, Coordinate, Coordinate, Coordinate)
type Space4d = DM.Map Location4d Bool

printImage image = unlines image'
  where    
    (xmin, xmax, ymin, ymax, zmin, zmax, wmin, wmax) = bounds image 
    image' = [ printLayer z | z <- [zmin..zmax] ]
    printLayer z = unlines $  ("z=" ++ show z):[ [ printCube $ DM.lookup (x, y, z, 0) image 
                              | x <- [xmin..xmax] ] | y <- [ymin..ymax] ]
    printCube (Just True)  = '#'
    printCube (Just False) = '.'
    printCube Nothing      = '.'

readImage = DM.fromList . concat . zipWith prepLine [0..]
  where
    prepLine y = zipWith (loc y) [0..]
    loc y x a = ((x, y, 0, 0), isActive a)

isActive :: Char -> Bool
isActive '#' = True
isActive  _  = False -- '.'

deltas = [-1,0,1]

neighbours3d :: Location4d -> [Location4d]
neighbours3d (x, y, z, _) = 
    [ (x+i, y+j, z+k, 0) | 
        i <- deltas, j <- deltas, k <- deltas,
        (i,j,k) /= (0,0,0)]        

neighbours4d :: Location4d -> [Location4d]
neighbours4d (x, y, z, w) = 
    [ (x+i, y+j, z+k, w+l) | 
        i <- deltas, j <- deltas, k <- deltas, l <- deltas,
        (i,j,k,l) /= (0,0,0,0)]

alive space l ne = length aliveCubes
    where
        aliveCubes = filter id cubesAround
        cubesAround = mapMaybe (`DM.lookup` space) (ne l)

aliveAround3d :: Space4d -> Location4d -> Int
aliveAround3d space l = alive space l neighbours3d
aliveAround4d :: Space4d -> Location4d -> Int
aliveAround4d space l = alive space l neighbours4d

step activeNeighbous getLocations space = DM.fromList newSpace 
    where
        newSpace = fmap (\x -> (x, newState x)) locations
        newState l = newState
            where
                iAmActive = Just True == DM.lookup l space
                iAmInactive = not iAmActive
                aliveCount = activeNeighbous space l
                newState
                  | iAmActive && aliveCount `elem` [2,3] = True
                  | iAmActive                            = False
                  | iAmInactive && aliveCount == 3       = True
                  | iAmInactive                          = False       
        locations = getLocations space

locations3d space = locations
  where
    (xr, yr, zr, _) = extendBounds space
    locations = [(x, y, z, 0) | x <- xr, y <- yr, z <- zr]

locations4d space = locations
  where
    (xr, yr, zr, wr) = extendBounds space
    locations = [(x, y, z, w) | x <- xr, y <- yr, z <- zr, w <- wr]

solution dm = length $ DM.elems $ DM.filter id dm 
solution1 space = solution $ iterate (step aliveAround3d locations3d) space !! 6
solution2 space = solution $ iterate (step aliveAround4d locations4d) space !! 6

main :: IO ()
main = do
    -- f <- readFile "sample"
    f <- readFile "input"

    let space4d = readImage $ lines f

    putStr "Task1: "
    print $ solution1 space4d

    putStr "Task2: "
    print $ solution2 space4d