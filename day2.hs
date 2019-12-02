import Text.ParserCombinators.Parsec
import qualified Data.Vector as DV
import Control.Monad (guard)

parseText' = sepBy nums (char ',') <* optional (char '\n')
nums = do
  n <- many1 digit
  return $ (read n :: Int)
parseFile = parse (parseText' <* eof) "(unknown)"

runProg xs = runProg' 0 xs
runProg' i xs = if opcode == 99
                then xs
                else runProg' next  xs'
  where
    opcode = xs DV.! i
    addr1  = xs DV.! (i + 1)
    addr2  = xs DV.! (i + 2)
    arg1   = xs DV.! addr1
    arg2   = xs DV.! addr2
    dst    = xs DV.! (i + 3)
    next   = i + 4
    operation 1 = (+)
    operation 2 = (*)
    xs' = xs DV.// [(dst, (operation opcode) arg1 arg2)]

setLastState xs = xs DV.// [(1, 12), (2, 2)]

bruteForce xs = head helper
  where
    helper = do
      noun <- [0..99]
      verb <- [0..99]
      let run = runProg (DV.fromList xs DV.// [(1,noun), (2,verb)])
      guard ((run DV.! 0) == 19690720)
      return $ 100 * noun + verb

main :: IO ()
main = do
  f <- readFile "input.txt"
  --f <- readFile "sample.txt"
  let parsed = parseFile f
  let task1 = fmap (runProg . setLastState . DV.fromList) parsed
  putStrLn "Task 1"
  print $ fmap (DV.! 0) task1
  let task2 = fmap (bruteForce) parsed
  putStrLn "Task 2"
  print $ task2
