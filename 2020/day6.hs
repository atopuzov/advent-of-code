import Text.ParserCombinators.Parsec
import Data.Either (fromRight)
import Data.List (foldr1, union, intersect)

answersParser = sepEndBy1 (many1 letter) (char '\n')
parseText = sepBy1 (answersParser) (string "\n")
parseFile = parse (parseText <* eof) "(unknown)"

main :: IO ()
main = do
  -- f <- readFile "sample"
  f <- readFile "input"

  let parsed = fromRight [] $ parseFile f

  let a = sum $ fmap (length . foldr1 union) parsed
  putStrLn $ "Task1: " ++ (show a)

  let b = sum $ fmap (length . foldr1 intersect) parsed
  putStrLn $ "Task2: " ++ (show b)
