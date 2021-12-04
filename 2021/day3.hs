{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Data.List

readInt :: Char -> Int
readInt '0' = 0
readInt '1' = 1

fromBinary nums = foldr (\x y -> x + 2*y) 0 $ reverse nums

task1 numbers = fromBinary gamma * fromBinary epsilon
  where
    halfBits = quot (length numbers) 2
    nums'' = (>halfBits) . sum <$> transpose numbers
    gamma   = fmap (\x -> if x then 1 else 0) nums''
    epsilon = fmap (\x -> if x then 0 else 1) nums''

task2 numbers = o2 * co2
  where
    o2  = fromBinary $ myFilter mostCommon  numbers 0
    co2 = fromBinary $ myFilter leastCommon numbers 0

    myFilter _ [x] _ = x
    myFilter f xs i = myFilter f filtered (i+1)
      where
        filtered = filter (\x -> x !! i == f xs i) xs

    onesAndZeros xs i = (ones, zeros)
      where
        ones = sum $ fmap (!! i) xs
        zeros = length xs - ones

    mostCommon nums p | ones == zeros = 1
                      | ones > zeros  = 1
                      | otherwise     = 0
      where
        (ones, zeros) = onesAndZeros nums p

    leastCommon nums p | ones == zeros = 0
                       | ones > zeros  = 0
                       | otherwise     = 1
      where
        (ones, zeros) = onesAndZeros nums p

main :: IO ()
main = do
    input <- readFile "input"
    let numbers = fmap readInt <$> lines input

    putStrLn "Task1:"
    print $ task1 numbers

    putStrLn "Task2:"
    print $ task2 numbers
