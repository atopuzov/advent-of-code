import Text.ParserCombinators.Parsec
import Data.Either (fromRight)

import Data.Functor.Contravariant
import Data.Semigroup
import qualified Data.List as DL
import qualified Data.Map as DM
import qualified Data.Set as DS


readInt :: String -> Int
readInt = read

number = do
    n <- many1 digit
    return $ readInt n

numberRange = do
    s1 <- number
    char '-'
    s2 <- number
    return (s1, s2)

ruleParser = do
    field <- many1 (alphaNum <|> char ' ')
    string ": "
    sepBy1 numberRange (string " or ")

ticketParser = sepBy1 number (char ',')

parseText = do
    rules <- sepEndBy ruleParser newline
    newline
    string "your ticket:\n"
    myTicket <- ticketParser
    newline
    newline
    string "nearby tickets:\n"
    nearbyTickets <- sepEndBy ticketParser newline
    return (rules, myTicket, nearbyTickets)

parseFile = parse parseText "(unknown)"

mergeRanges (a@(ax, ay):b@(bx, by):rest)
    | ax <= bx && bx <= ay || ay + 1 == bx = mergeRanges ((ax, max ay by):rest)
    | otherwise = a:mergeRanges (b:rest)
mergeRanges o = o

inRange x (a, b) = x >= a && x <= b
inRanges ranges val = getAny $ foldMap (Any . inRange val) ranges

sol1 rules nearbyTickets = sum $ filter (not . inRanges mergedRanges) nearBy
    where 
        nearBy = concat nearbyTickets
        mergedRanges = mergeRanges $ DL.sort $ concat rules

isValid values rule = getAll $ foldMap (All . inRanges rule) values

validRules rulesMap values = DS.fromList . DM.keys $ DM.filter id valid
    -- go trough each rule
    where
        valid = DM.map (isValid values) rulesMap

validateFields rules fields = DM.map (validRules rulesMap) fieldsMap
    -- go trough each field
    where
        rulesMap = DM.fromList $ zip [0..] rules
        fieldsMap = DM.fromList $ zip [0..] fields

isValidTicket ranges vals = getAll $ foldMap (All . inRanges ranges) vals 

solver [] _ _ = []
solver ((f,_):rest) maps removed = (f,x) : solver rest maps removed'
    where
        validRules = maps DM.! f
        removed' = DS.union removed validRules
        [x] = DS.toList $ DS.difference validRules removed

sol2 rules myTicket nearByTickets = product $ fmap (\x -> myTicket !! x) locs 
    where
        mergedRanges = mergeRanges $ DL.sort $ concat rules
        validTickets = filter (isValidTicket mergedRanges) nearByTickets
        fields = DL.transpose validTickets
        validated = validateFields rules fields
        order = DM.map length validated
        removalOrder = DL.sortBy (\(_,x) (_,y) -> compare x y) $ DM.assocs order
        mapper = solver removalOrder validated DS.empty
        locs = fst <$> filter (\x -> snd x < 6) mapper

main :: IO ()
main = do
    -- f <- readFile "sample"
    -- f <- readFile "sample2"
    f <- readFile "input"

    let (rules, myTicket, nearByTickets) = fromRight ([], [], []) $ parseFile f
 
    putStr "Task1: "
    print $ sol1 rules nearByTickets

    putStr "Task1: "
    print $ sol2 rules myTicket nearByTickets