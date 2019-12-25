import qualified Data.Map as DM
import qualified Data.Set as DS
import Data.Maybe (fromMaybe)

data Cell = Bug | Empty deriving (Show, Eq, Ord)

evolve image = DM.mapWithKey evolve image
  where
    evolve k Bug   | nBugs k == 1 = Bug
    evolve k Empty | nBugs k `elem` [1,2] = Bug
    evolve _ _ = Empty
    nBugs k = length $ filter (==Bug) $ fmap (flip (DM.findWithDefault Empty) image) (nLocs k)
    nLocs (x,y,_) = [(x,y-1,0), (x,y+1,0), (x+1,y,0), (x-1,y,0)]

firstDup xs = firstDup' xs DS.empty
firstDup' [] _ = Nothing
firstDup' (x:xs) s =
  if DS.member x s
  then Just x
  else firstDup' xs (DS.insert x s)

calcBioDiversity image = DM.foldrWithKey biodiversity 0 image
  where
    biodiversity (x, y, _) Bug   acc = acc + 2 ^ (x + y * 5)
    biodiversity _         Empty acc = acc

adjacent (0,0,n) = [(1,0,n), (0,1,n), (2,1,n-1), (1,2,n-1)]
adjacent (1,0,n) = [(0,0,n), (2,0,n), (1,1,n),   (2,1,n-1)]
adjacent (2,0,n) = [(1,0,n), (3,0,n), (2,1,n),   (2,1,n-1)]
adjacent (3,0,n) = [(2,0,n), (4,0,n), (3,1,n),   (2,1,n-1)]
adjacent (4,0,n) = [(3,0,n), (4,1,n), (2,1,n-1), (3,2,n-1)]
adjacent (0,1,n) = [(0,0,n), (1,1,n), (0,2,n),   (1,2,n-1)]
adjacent (1,1,n) = [(1,0,n), (0,1,n), (2,1,n),   (1,2,n)]
adjacent (2,1,n) = [(2,0,n), (1,1,n), (3,1,n),   (0,0,n+1), (1,0,n+1), (2,0,n+1), (3,0,n+1), (4,0,n+1)]
adjacent (3,1,n) = [(3,0,n), (2,1,n), (4,1,n),   (3,2,n)]
adjacent (4,1,n) = [(4,0,n), (3,1,n), (4,2,n),   (3,2,n-1)]
adjacent (0,2,n) = [(0,1,n), (1,2,n), (0,3,n),   (1,2,n-1)]
adjacent (1,2,n) = [(1,1,n), (0,2,n), (1,3,n),   (0,0,n+1), (0,1,n+1), (0,2,n+1), (0,3,n+1), (0,4,n+1)]
adjacent (2,2,n) = []
adjacent (3,2,n) = [(3,1,n), (4,2,n), (3,3,n),   (4,0,n+1), (4,1,n+1), (4,2,n+1), (4,3,n+1), (4,4,n+1)]
adjacent (4,2,n) = [(4,1,n), (3,2,n), (4,3,n),   (3,2,n-1)]
adjacent (0,3,n) = [(0,2,n), (1,3,n), (0,4,n),   (1,2,n-1)]
adjacent (1,3,n) = [(1,2,n), (0,3,n), (2,3,n),   (1,4,n)]
adjacent (2,3,n) = [(1,3,n), (3,3,n), (2,4,n),   (0,4,n+1), (1,4,n+1), (2,4,n+1), (3,4,n+1 ), (4,4,n+1)]
adjacent (3,3,n) = [(3,2,n), (2,3,n), (4,3,n),   (3,4,n)]
adjacent (4,3,n) = [(4,2,n), (3,3,n), (4,4,n),   (3,2,n-1)]
adjacent (0,4,n) = [(0,3,n), (1,4,n), (1,2,n-1), (2,3,n-1)]
adjacent (1,4,n) = [(1,3,n), (0,4,n), (2,4,n),   (2,3,n-1)]
adjacent (2,4,n) = [(2,3,n), (1,4,n), (3,4,n),   (2,3,n-1)]
adjacent (3,4,n) = [(3,3,n), (2,4,n), (4,4,n),   (2,3,n-1)]
adjacent (4,4,n) = [(4,3,n), (3,4,n), (3,2,n-1), (2,3,n-1)]
adjacent (_,_,_) = error "Not a valid tile!!!"

loc' y x '#' = ((x, y, 0), Bug)
loc' y x '.' = ((x, y, 0), Empty)
prepLine' y line = zipWith (loc' y) [0..] line
readImage' = concat . zipWith prepLine' [0..]

printImage l dm = unlines image
  where
    printPixel (2,2,_) = '?'
    printPixel pixel  = case DM.lookup pixel dm of
                          Just Bug   -> '#'
                          Just Empty -> ' '
                          _          -> ' '
    image = [ [ printPixel (x, y, l) | x <- [0..4] ] | y <- [0..4] ]

evolve' image = DM.mapWithKey evolve image
  where
    evolve k Bug   | nBugs k == 1         = Bug
    evolve k Empty | nBugs k `elem` [1,2] = Bug
    evolve _ _ = Empty
    nBugs k = length $ filter (==Bug) $ fmap (flip (DM.findWithDefault Empty) image) (adjacent k)

createFoldedSpace depth = do
  x <- [0..4]
  y <- [0..4]
  l <- [-depth..depth]
  return ((x,y,l), Empty)

main :: IO ()
main = do
  f <- readFile "input.txt"
  -- f <- readFile "sample.txt"
  let image = DM.fromList $ readImage' $ lines f
  let all = iterate evolve image
  let repeating = fromMaybe DM.empty $ firstDup all

  putStrLn "Part1:"
  print $ calcBioDiversity repeating

  putStrLn "Part2:"
  let image' = DM.union image $ DM.fromList $ createFoldedSpace 201
  let all' = iterate evolve' image'
  let after' = all' !! 200
  print $ length $ DM.elems $ DM.filter (==Bug) after'
