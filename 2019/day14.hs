{-# LANGUAGE FlexibleContexts #-}
import Data.Either (fromRight)
import Data.Maybe (fromMaybe)
import Text.ParserCombinators.Parsec
import qualified Data.Map as DM

eol = char '\n'
parseText = sepEndBy lineParser eol
lineParser = do
  src <- sepBy itemParser (string ", ")
  string " => "
  (q, n) <- itemParser
  return (n, (q, src))
itemParser = do
  q <- many1 digit
  char ' '
  n <- many1 letter
  return (read q :: Int, n)
parseFile = parse (parseText <* eof) "(unknown)"

produce rcpt storage []                   = storage
produce rcpt storage ((want, "ORE"):rest) = produce rcpt storage' rest
  where
    storage' = DM.insertWith (+) "ORE" want storage
produce rcpt storage ((want,  chem):rest) =
  if (need > 0)
  then produce rcpt storage' (reactants ++ rest)
  else produce rcpt storage'' rest
  where
    (produced, ingredients) = rcpt DM.! chem
    have = DM.findWithDefault 0 chem storage
    need = (want - have)
    reactions = ceiling $ fromIntegral need / fromIntegral produced
    storage'  = DM.insertWith (+) chem (reactions * produced - want) storage
    storage'' = DM.insertWith (+) chem (-want) storage
    reactants = fmap (\(q, c) -> (q * reactions, c)) ingredients

oreForFuel rcpt fuel = (produce rcpt DM.empty [(fuel,"FUEL")]) DM.! "ORE"

sol1 rcpt = oreForFuel rcpt 1

sol2 f ore = bs 0 ore
  where
    bs l h
      | l + 1 == h  = l
      | calc <= ore = bs mid h
      | otherwise   = bs l mid
      where
        mid = l + (h - l) `div` 2
        calc = f mid

main :: IO ()
main = do
  f <- readFile "input.txt"
  -- f <- readFile "sample1.txt"
  -- f <- readFile "sample2.txt"
  -- f <- readFile "sample3.txt"
  -- f <- readFile "sample4.txt"
  -- f <- readFile "sample5.txt"
  let parsed = fromRight [] $ parseFile f
  let rcpt = DM.fromList parsed
  putStrLn "Part1:"
  print $ sol1 rcpt
  putStrLn "Part2:"
  print $ sol2 (oreForFuel rcpt) 1000000000000
