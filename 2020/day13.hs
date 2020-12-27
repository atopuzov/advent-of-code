{-# LANGUAGE TupleSections #-}
import Text.ParserCombinators.Parsec
import Data.Either (fromRight)
import Data.Maybe (mapMaybe, catMaybes)

scheduleParser = do
    b <- many1 alphaNum
    return $ if b == "x" then Nothing else Just (read b :: Int)

parseText = do
    t <- many1 digit
    char '\n'
    s <- sepBy1 scheduleParser (char ',')
    char '\n'
    return (read t :: Int, s)

parseFile = parse (parseText <* spaces <* eof) "(unknown)"

nextTimestamp t x = (t `div` x + 1) * x
sol1 timestamp schedule = (timestamp' - timestamp) * busId
  where
    possible = mapMaybe (fmap (flip (,) <*> nextTimestamp timestamp)) schedule
    (timestamp', busId) = minimum possible

sol2 schedule = fst $ foldr f (0, 1) schedule'
  where
    schedule' = reverse . catMaybes $ zipWith (\x i -> (i,) <$> x) schedule [0..]
    f (offset, i) (base, step) = (base', step * i)
      where
        base' = until vrfy (+step) base
        vrfy x = (x + offset) `mod` i == 0

main = do
    -- f <- readFile "sample"
    f <- readFile "input"

    let (timestamp, schedule) = fromRight (0, []) $ parseFile f

    putStr "Task1: "
    print $ sol1 timestamp schedule

    putStr "Task2: "
    print $ sol2 schedule
