{-# LANGUAGE DeriveFunctor #-}
import Text.ParserCombinators.Parsec
import Data.Either ( fromRight )
import Debug.Trace
import Data.List

lineParser :: Parser String
lineParser = many1 (oneOf "()[]{}<>")

parseText :: Parser [String]
parseText = sepEndBy1 lineParser (char '\n')

parseFile = parse (parseText <* eof) "(unknown)"

isClosing :: Char -> Bool
isClosing x = x `elem` ")]}>"

isOpening :: Char -> Bool
isOpening x = x `elem` "([{<"

validClosing :: Char -> Char -> Bool
validClosing '(' ')' = True
validClosing '[' ']' = True
validClosing '{' '}' = True
validClosing '<' '>' = True
validClosing  _   _  = False

isValidLine line = go line []
  where
    go [] _ = (True, 0)
    go (c:cs) []
      | isOpening c = go cs [c]
      | otherwise   = (False, illegalCharScore c)
    go (c:cs) (b:bs)
      | validClosing b c = go cs bs
      | isOpening c      = go cs (c:b:bs)
      | otherwise        = (False, illegalCharScore c)

    illegalCharScore ')' = 3
    illegalCharScore ']' = 57
    illegalCharScore '}' = 1197
    illegalCharScore '>' = 25137
    illegalCharScore  _  = 0

task1 x = sum $ fmap snd $ filter (not . fst) $ fmap isValidLine x

completeLine :: [Char] -> [Char]
completeLine line = closingFor <$> go line []
  where
    go [] closing = closing
    go (c:cs) [] = go cs [c]
    go (c:cs) (b:bs)
      | validClosing b c = go cs bs
      | otherwise        = go cs (c:b:bs)

    closingFor '(' = ')'
    closingFor '[' = ']'
    closingFor '{' = '}'
    closingFor '<' = '>'
    closingFor x = x

task2 lins = scores !! (length scores `div` 2)
  where
    scores = sort $ fmap (scoreClosing . completeLine) validLins
    validLins = filter (fst . isValidLine) lins
    scoreClosing = foldl (\t n -> t * 5 + closingScore n) 0

    closingScore ')' = 1
    closingScore ']' = 2
    closingScore '}' = 3
    closingScore '>' = 4
    closingScore  _  = 4


main :: IO ()
main = do
  f <- readFile "input"
  let i = fromRight [] $ parseFile f

  putStrLn "Task1:"
  print $ task1 i

  putStrLn "Task2:"
  print $ task2 i
