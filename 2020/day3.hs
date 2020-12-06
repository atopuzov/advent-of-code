import qualified Data.Map as DM
import Data.Maybe ( isJust )


isTree '#' = 1
isTree  _  = 0

bounds dm = (xmax,ymax)
  where
    k = DM.keys dm
    xs = fmap fst k
    ys = fmap snd k
    xmax = maximum xs
    ymax = maximum ys

getPixel dm loc = fmap isTree $ DM.lookup (fixLoc loc) dm
    where
        (xmax, ymax) = bounds dm
        fixLoc (x, y) = (x `mod` (xmax + 1), y)

nextLoc slope (x, y) = (x + fst slope, y + snd slope)

loc y x a = ((x, y), a)
prepLine y line = zipWith (loc y) [0..] line
readImage = concat . zipWith prepLine [0..]


slopes = [(1,1), (3,1), (5,1), (7,1), (1,2)]

main :: IO ()
main = do
    -- f <- readFile "sample"
    f <- readFile "input"

    let terrain =  DM.fromList $ readImage $ lines f

    let locs = iterate (nextLoc (3,1)) (0, 0) 
    print $ fmap sum $ sequence $ takeWhile isJust $ fmap (getPixel terrain) locs
    
    putStrLn "Task2: "
    let val1 = fmap sum $ sequence $ takeWhile isJust $ fmap (getPixel terrain) $ iterate (nextLoc (1,1)) (0, 0)
    let val2 =  fmap sum $ sequence $ takeWhile isJust $ fmap (getPixel terrain) $ iterate (nextLoc (3,1)) (0, 0)
    let val3 = fmap sum $ sequence $ takeWhile isJust $ fmap (getPixel terrain) $ iterate (nextLoc (5,1)) (0, 0)
    let val4 = fmap sum $ sequence $ takeWhile isJust $ fmap (getPixel terrain) $ iterate (nextLoc (7,1)) (0, 0)
    let val5 =  fmap sum $ sequence $ takeWhile isJust $ fmap (getPixel terrain) $ iterate (nextLoc (1,2)) (0, 0)
    print $ fmap product $ sequence [val1, val2, val3, val4, val5]


    putStrLn "End"