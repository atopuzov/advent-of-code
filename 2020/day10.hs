{-# LANGUAGE TupleSections #-}
import qualified Data.List as DL
import qualified Data.Map as DM


diffs nums = (zipWith (-) (nums) (0:nums)) ++ [3]
part1 nums = ones * threes
    where
        df = diffs $ DL.sort nums
        ones   = length $ filter (==1) df
        threes = length $ filter (==3) df

part2 xs = paths DM.! 0
    where
        paths = foldr f dm nums
        nums = init $ numbers
        numbers = DL.sort (0:maximum xs + 3:xs)
        f x dm' = DM.union dm'' dm'
            where
                dm'' = DM.singleton x $ sum paths
                paths = [ DM.findWithDefault 0 (x+i) dm' | i <- [1..3]]           
        dm = DM.fromAscList $ fmap (,1) numbers


main = do
    -- f <- readFile "sample"
    -- f <- readFile "sample2"
    f <- readFile "input"  
    let numbers = fmap (read :: String -> Int) (lines f)


    putStr "Task1: "
    print $ part1 numbers

    putStr "Task2: "
    print $ part2 numbers

