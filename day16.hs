import Data.Char (digitToInt, intToDigit)

startPatt = [0, 1, 0, -1]
calcPatForOutput 1 = startPatt
calcPatForOutput n = concat $ zipWith replicate (repeat n) startPatt
calcPhase input patt = zipWith (\a b -> (signum b) * (getTenDigit $ a*b)) input (tail $ cycle patt)

getTenDigit = (`mod` 10) . abs

calc input = do
  i <- [1..length input]
  let x = calcPhase input (calcPatForOutput i)
  return $ getTenDigit $ sum x

getInput = fmap (fromIntegral . digitToInt)

doPhases n input = head $ drop n $ iterate calc input
sol1 input = fmap intToDigit $ take 8 $ doPhases 100 input


pa n i = take n $ cycle (calcPatForOutput i)

main :: IO ()
main = do
  f <- readFile "input.txt"
  let input = head (lines f)
  putStrLn "Part1:"
  putStrLn $ sol1 $ getInput input
