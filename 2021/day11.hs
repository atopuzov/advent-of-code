{-# LANGUAGE TupleSections #-}
import Text.ParserCombinators.Parsec
import Data.Either ( fromRight )
import qualified Data.Map as DM
import qualified Data.Set as DS
import Data.Maybe (mapMaybe)

num :: Parser Int
num = do
  n <- digit
  return (read [n] :: Int)

parseText :: Parser [[Int]]
parseText = sepEndBy1 (many1 num) (char '\n')

parseFile :: String -> Either ParseError [[Int]]
parseFile = parse (parseText <* eof) "(unknown)"

type Pos = (Int, Int)
type SP = DS.Set Pos
type MP = DM.Map Pos Int

processInput :: [[Int]] -> MP
processInput = DM.fromList . concat . zipWith (\y -> zipWith (\x v -> ((x, y), v)) [0..] ) [0..]

flashOcto :: MP -> (MP, Int)
flashOcto octo = incFlash octo DS.empty flashing
  where
    flashing = DM.keysSet $ DM.filter (>9) octo

incFlash :: MP -> SP -> SP -> (MP, Int)
incFlash octo flashed flashing =
    if DS.null flashing
    then (octo'', DS.size flashed)
    else incFlash octo' flashed' flashing'
  where
    octo' = DS.foldr (\a dm -> DM.unionWith (+) dm (neighbours a)) octo flashing
    nowFlashing = DM.keysSet $ DM.filter (>9) octo'

    flashed' = DS.union flashed flashing
    flashing' = DS.difference nowFlashing flashed'

    neighbours (x, y) = DM.fromList $ zip (filter (`DM.member` octo) [(x-1,y-1), (x,y-1), (x+1,y-1), (x-1, y), (x+1, y), (x-1,y+1), (x,y+1), (x+1,y+1)]) (repeat 1)

    resetFlashing = DM.fromList $ DS.toList $ DS.map (,0) flashed
    octo'' = DM.unionWith (\ _ x -> x) octo resetFlashing

step :: (MP, Int) -> (MP, Int)
step (dm, flashes) = (dm'', flashes + flashes')
  where
    dm' = DM.map (+1) dm
    (dm'', flashes') = flashOcto dm'

task1 :: MP -> Int
task1 dm = snd $ steps !! 100
  where
    steps = iterate step (dm, 0)

task2:: MP -> Int
task2 dm = length $ takeWhile (anyNonZero . fst) steps
  where
    steps = iterate step (dm, 0)
    anyNonZero k = any (/=0) $ DM.elems k

main :: IO ()
main = do
  f <- readFile "input"
  let i = fromRight [] $ parseFile f
  let dm = processInput i

  putStrLn "Task1:"
  print $ task1 dm

  putStrLn "Task2:"
  print $ task2 dm
