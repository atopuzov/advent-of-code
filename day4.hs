import Data.List (group)

low = 138241
high = 674034
pass = [low..high]

valid x = (not s) && f
  where
    f = or $ fmap fst checks
    s = or $ fmap snd checks
    checks = valid' . show $ x

valid' (a:b:rest) = (a==b, a>b):valid' (b:rest)
valid' (a:[]) = []
valid' [] = []

valid'' xs = any (\x -> length x == 2) $ group $ show xs

main :: IO ()
main = do
  let validPasswords = filter valid pass
  putStrLn "Part1:"
  print $ length validPasswords
  let moreValidPasswords = filter valid'' validPasswords
  putStrLn "Part2:"
  print $ length moreValidPasswords
