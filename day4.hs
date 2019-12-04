import Data.List (group)

low = 138241
high = 674034

pass = (dropWhile (<low) . takeWhile (<=high))
  [100000 * a +
    10000 * b +
     1000 * c +
      100 * d +
       10 * e +
            f |
         a <- [1..6],
         b <- [a..9],
         c <- [b..9],
         d <- [c..9],
         e <- [d..9],
         f <- [e..9]]

valid x =  groups && monotonic
  where
    (groups, monotonic) = foldr fun (False, True) checks
    fun (a,b) (aa, ab) = (a || aa, b && ab)
    checks = valid' . show $ x
    valid' xs = zipWith (\a b -> (a == b, a <= b)) xs (tail xs)

valid'' xs = any (\x -> length x == 2) $ group $ show xs

main :: IO ()
main = do
  let validPasswords = filter valid pass
  putStrLn "Part1:"
  print $ length validPasswords
  let moreValidPasswords = filter valid'' validPasswords
  putStrLn "Part2:"
  print $ length moreValidPasswords
