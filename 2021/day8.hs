{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Text.ParserCombinators.Parsec
import Data.Either ( fromRight )
import qualified Data.Set as DS
import qualified Data.Map as DM


type Segment = DS.Set Char

segment :: Parser Segment
segment = do
  s <- many1 $ oneOf "abcdefg"
  return $ DS.fromList s

segments :: Parser [Segment]
segments = sepEndBy1 segment (char ' ')

inputParser :: Parser ([Segment], [Segment])
inputParser = do
  segs <- segments
  char '|'
  spaces
  out <- segments
  return (segs, out)

parseText = sepEndBy1 inputParser (char '\n')

parseFile = parse (parseText <* eof) "(unknown)"

task1 lns = length $ ofSizes [2,4,3,7] outputs
  where
    outputs = concatMap snd lns
    ofSizes sizes s = filter ((`elem` sizes) . DS.size) s

genWireMap :: [Char] -> DM.Map (DS.Set Char) Int
genWireMap [a,b,c,d,e,f,g] = DM.fromList [
    (DS.fromList [a,b,c,e,f,g], 0),
    (DS.fromList [c,f], 1),
    (DS.fromList [a,c,d,e,g], 2),
    (DS.fromList [a,c,d,f,g], 3),
    (DS.fromList [b,c,d,f], 4),
    (DS.fromList [a,b,d,f,g], 5),
    (DS.fromList [a,b,d,e,f,g], 6),
    (DS.fromList [a,c,f], 7),
    (DS.fromList [a,b,c,d,e,f,g], 8),
    (DS.fromList [a,b,c,d,f,g], 9)
  ]

task2 = sum . fmap decode
  where
    decode (wires, output) = foldl (\b a -> a + b*10) 0 nums
      where
        nums = fmap (wireMap DM.!) output
        wireMap = genWireMap $ fmap (head . DS.toList) [a,b,c,d,e,f,g]
        ofSize n s = filter ((==n) . DS.size) s

        a = DS.difference acf cf
        b = DS.difference bcdf cdf
        c = DS.difference cf f
        d = DS.intersection bd dg
        e = DS.difference cde cd
        f = DS.difference fg g
        g = DS.difference dg d

        ab = DS.union a b
        bd = DS.difference bcdf cf
        cf = num1
        cd = DS.difference cdf f
        dg = DS.difference adg a
        fg = DS.difference abfg ab

        acf = num7
        cdf = DS.union cf d
        cde = DS.difference abcdefg abfg
        adg = foldr1 DS.intersection num235

        abfg = foldr1 DS.intersection num069
        bcdf = num4

        abcdefg = num8

        [num1] = ofSize 2 wires
        [num4] = ofSize 4 wires
        [num7] = ofSize 3 wires
        [num8] = ofSize 7 wires
        num235 = ofSize 5 wires
        num069 = ofSize 6 wires


main :: IO ()
main = do
  f <- readFile "input"
  let lns = fromRight [([], [])] $ parseFile f

  putStrLn "Task1: "
  print $ task1 lns

  putStrLn "Task2: "
  print $ task2 lns
