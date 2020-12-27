{-# LANGUAGE BangPatterns #-}
import Text.ParserCombinators.Parsec
import Data.Either (fromRight)
import qualified Data.IntMap.Strict as DM

readInt :: String -> Int
readInt = read

number = readInt <$> many1 digit

parseText = sepBy number (char ',')
parseFile = parse (parseText <* spaces <* eof) "(unknown)"

step (!spoken, !last, !turn) = (spoken', number, turn + 1)
    where
        !number = case DM.lookup last spoken of
            Nothing -> 0
            Just x -> turn - x
        !spoken' = DM.insert last turn spoken

solution iterations numbers  = n
    where
        (_, n, _) = foldr (const step) initialState [1..iterations - length numbers]
        spoken = DM.fromList $ zip numbers [1..]
        initialState = (spoken, last numbers, length numbers)

sol1 = solution 2020
sol2 = solution 30000000

main :: IO ()
main = do
    -- f <- readFile "sample"
    f <- readFile "input"
    let parsed = fromRight [] $ parseFile f

    putStr "Task1: "
    print $ sol1 parsed

    putStr "Task2: "
    print $ sol2 parsed