readInt :: String -> Int
readInt = read

task1 numbers = foldr helper 0 pairs
    where
        pairs = zip (tail numbers) numbers
        helper = \(x, y) acc -> if x > y then acc + 1 else acc

task2 numbers = task1 msums
    where
        msums = helper numbers
        helper (x:y:z:r) = (x+y+z) : helper (y:z:r)
        helper _         = []

main :: IO ()
main = do
    input <- readFile "input"
    let numbers = readInt <$> lines input

    putStr "Task1: "
    print $ task1 numbers

    putStr "Task2: "
    print $ task2 numbers



