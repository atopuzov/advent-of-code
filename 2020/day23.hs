{-# LANGUAGE BangPatterns #-}
import qualified Data.IntMap as DM
import Data.Foldable (foldr')
import Data.Char (digitToInt, intToDigit)

getMapping cups = (firstCup, mapping)
  where
    mapping = DM.fromList $ lastPair:zip cups (tail cups)
    firstCup = head cups
    lastCup = last cups
    lastPair = (lastCup, firstCup)

findCupG maxCup 0 dm picked = findCupG maxCup maxCup dm picked
findCupG maxCup c dm picked
  | c `elem` picked = findCupG maxCup (c-1) dm picked
  | otherwise = c

step findCup (current, dm) = (nextCup, dm')
  where
    !picked1 = dm DM.! current
    !picked2 = dm DM.! picked1
    !picked3 = dm DM.! picked2
    !nextCup = dm DM.! picked3
    !destinationCup = findCup (current - 1) dm [picked1, picked2, picked3]
    !destinationCupNext = dm DM.! destinationCup
    !mapUpdates = DM.fromList [(current, nextCup), (picked3, destinationCupNext),
                              (destinationCup, picked1)]
    !dm' = DM.union mapUpdates dm

sol1 labels = fmap intToDigit cupsAfterOne
  where
    cupsAfterOne = cps (cupMapping DM.! 1)
    cps 1 = []
    cps n = n : cps (cupMapping DM.! n)
    (_, cupMapping) = foldr' (const step1) initialState [1..100]
    cups = fmap digitToInt labels
    initialState = getMapping cups
    findCup1 = findCupG 9
    step1 = step findCup1

sol2 labels = cup1 * cup2
  where
    cup1 = cupMapping DM.! 1
    cup2 = cupMapping DM.! cup1
    (_, cupMapping) = foldr' (const step2) initialState [1..10000000]
    cups = fmap digitToInt labels ++ [10..1000000]
    initialState = getMapping cups
    findCup2 = findCupG 1000000
    step2 = step findCup2

main :: IO ()
main = do
  let lbls = "389125467"

  putStr "Task1: "
  print $ sol1 lbls

  putStr "Task2: "
  print $ sol2 lbls
