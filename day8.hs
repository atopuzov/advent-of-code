import Data.Either (fromRight)
import Text.ParserCombinators.Parsec
import Data.Char (digitToInt)
import Data.List

parseText = endBy image (char '\n')

image = many1 layer

layer = do
  rows <- count 6 row
  return rows

row = do
  row <- count 25 num
  return row

num = do
  n <- digit
  return $ digitToInt n

parseFile = parse (parseText <* eof) "(unknown)"

countDigits xs = foldr counter (0,0,0) $ concat xs
  where
    counter a (x,y,z) | a == 0 = (x+1,y,z)
    counter a (x,y,z) | a == 1 = (x,y+1,z)
    counter a (x,y,z) | a == 2 = (x,y,z+1)
    counter a (x,y,z) = (x,y,z)

sol1 xs = ones * twos
  where
    ((_, ones, twos):_) = sort $ fmap countDigits xs

-- 0 black
-- 1 white
-- 2 transparent
convertPixel 0 = ' '
convertPixel 1 = '*'
convertPixel 2 = '_'
printImage xs = sequence_ $ fmap (putStrLn . fmap convertPixel) xs

combinePixel 2 x = x
combinePixel x 2 = x
combinePixel a b = b
combineRow row row' = zipWith combinePixel row row'
combineLayers layers = foldr1 (zipWith combineRow) $ reverse layers

main :: IO ()
main = do
  f <- readFile "input.txt"
  let parsed = fromRight [] $ parseFile f

  putStrLn "Part1:"
  print $ fmap (sol1) parsed

  putStrLn "Part2:"
  printImage . combineLayers $ parsed !! 0
