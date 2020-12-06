import Data.List (sort, zipWith, zipWith3)
import Data.Maybe (isJust, fromJust)

decodePass p = (row, column)
    where
        row = decodeRow $ take 7 p
        column = decodeColumn $ drop 7 p

getSeatId (x, y) = x * 8 + y

getLow low high = high - (high - low) `div` 2
getHigh low high = low + (high - low) `div` 2

decodeRow = decodeRow' 0 127
decodeRow' low high ('F':[]) = low
decodeRow' low high ('F':xs) = decodeRow' low (getHigh low high) xs

decodeRow' low high ('B':[]) = getLow low high
decodeRow' low high ('B':xs) = decodeRow' (getLow low high) high xs

decodeColumn = decodeColumn' 0 7
decodeColumn' low high ('L':[]) = low
decodeColumn' low high ('L':xs) = decodeColumn' low (getHigh low high) xs

decodeColumn' low high ('R':[]) = getLow low high        
decodeColumn' low high ('R':xs) = decodeColumn' (getLow low high) high xs

miss (x:y:r) = if y == x + 1
                then miss (y:r)
                else x + 1

main :: IO ()
main = do
    f <- readFile "input"
    let passes = lines f
    let seatIds = fmap (getSeatId . decodePass) passes

    putStr "Task1: "
    print $ maximum seatIds

    putStr "Task2: "
    let s = sort seatIds
    print $ miss s