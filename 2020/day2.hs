import Text.ParserCombinators.Parsec
import Data.Either (fromRight)

num :: Parser Int
num = do
  n <- many1 digit
  return $ read n

eol = char '\n'
parseText = sepEndBy lineParser eol

lineParser = do
  mino <- num
  char '-'
  maxo <- num 
  char ' '
  lett <- letter
  char ':'
  char ' '
  pass <- many1 letter
  return (mino, maxo, lett, pass)

parseFile = parse (parseText <* eof) "(unknown)"

isValid (mino, maxo, lett, pass) = (mino <= occur) && (maxo >= occur)
  where
    onlyLetter = filter (==lett) pass
    occur = length onlyLetter

isValid2 (idx1, idx2, lett, pass) = 
    (let1 == lett && let2 /= lett) || 
    (let2 == lett && let1 /= lett)
  where
    let1 = pass !! (idx1-1)
    let2 = pass !! (idx2-1)

main :: IO ()
main = do
  f <- readFile "input"
  let parsed = fromRight [] $ parseFile f
  let valid = filter isValid parsed
  putStr "Task1: "
  print $ length valid


  let valid2 = filter isValid2 parsed
  putStr "Task2: "
  print $ length valid2
