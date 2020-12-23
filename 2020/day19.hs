import Text.ParserCombinators.Parsec
import qualified Data.IntMap as DM
import Data.Either (fromRight)
import Data.Maybe (catMaybes, isJust, fromJust)

data Rule = CharRule Char | Rules [[Int]] deriving Show

readInt :: String -> Int 
readInt = read

readNumber = do
  n <- many1 digit 
  return $ readInt n

numbers = spaces >> sepEndBy1 readNumber (char ' ')
numberRule = Rules <$> sepBy1 numbers (char '|')
charRule = between (char '"') (char '"') (CharRule <$> letter)

parseRule = do
    num <- readNumber
    char ':'
    spaces
    rules <- numberRule <|> charRule
    return (num, rules)

parseRules = DM.fromList <$> sepEndBy1 parseRule newline 

parseText = do
  rules <- parseRules
  newline
  text <- sepEndBy1 (many1 letter) newline
  return (rules, text)

parseFile = parse (parseText <* spaces <* eof) "(unknown)"

isValidString rulesMap str = Just "" `elem` results
  where
    results = testString rulesMap 0 str 

testString :: DM.IntMap Rule -> Int -> String -> [Maybe String]
testString rulesMap ruleNum str = filter isJust isValid
  where
    rule = rulesMap DM.! ruleNum
    isValid = testRule rulesMap rule str

testRule :: DM.IntMap Rule -> Rule -> String -> [Maybe String]
testRule _        (CharRule x) (c:r)
   | x == c                       = [Just r]
   | otherwise                    = [Nothing]
testRule _        (CharRule x) t  = [Nothing]
testRule rulesMap (Rules rules) t = 
  do
    rule <- rules
    testRules rulesMap t rule

testRules rulesMap t []     = [Just t]
testRules rulesMap t (x:xs) = 
  do
    t <- testString rulesMap x t
    testRules rulesMap (fromJust t) xs

solution rules text = length $ filter (isValidString rules) text

main :: IO ()
main = do
    -- f <- readFile "sample"
    -- f <- readFile "sample2"
    f <- readFile "input"

    let (rules, text) = fromRight (DM.empty, []) $ parseFile f

    putStr "Task1: "
    print $ solution rules text

    fix <- readFile "fix"
    let rulesFix = fromRight DM.empty $ parse parseRules "" fix
    let fixedRules = DM.union rulesFix rules
    
    putStr "Task2: "
    print $ solution fixedRules text