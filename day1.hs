
fuel m = (m `div` 3) - 2

fuel' m = if f < 0
          then 0
          else f
  where
    f = fuel m

solve2 x = sum . tail . takeWhile (>0) $ iterate fuel' x

main :: IO ()
main = do
  con <- readFile "input.txt"
  let modules = fmap read $ lines con
  let fuelSum = sum $ fmap fuel modules
  putStrLn "Part 1:"
  print fuelSum

  let fuelSum2 = sum $ fmap solve2 modules
  putStrLn "Part 2:"
  print fuelSum2
