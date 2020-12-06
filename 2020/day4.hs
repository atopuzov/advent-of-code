import Text.ParserCombinators.Parsec
import Data.Either (fromRight)
import qualified Data.Map as DM
import qualified Data.Set as DS
import Data.Maybe (isJust)

keyValSep = char ':'

val = many1 (alphaNum <|> char '#')

pp = do -- height
  c1 <- letter
  c2 <- letter
  c3 <- letter
  keyValSep
  v <- val
  return ([c1,c2,c3], v)

requiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
f p x = x `DM.member` p
validatePassport p = all (f p) requiredFields

vaildByr byr = 1920 <= year && year <= 2002
  where
    year = read byr

vaildIyr iyr = 2010 <= year && year <= 2020
  where
    year = read iyr

vaildEyr eyr = 2020 <= year && year <= 2030
  where
    year = read eyr

validHgt hgt = case reads hgt :: [(Int, String)] of 
    [(height, "cm")] -> 150 <= height && height <= 193
    [(height, "in")] -> 59 <= height && height <= 76
    _ -> False

isHex c = c `elem` (['0'..'9'] ++ ['a'..'f'])

validHcl ('#':xs) = length xs == 6 && all isHex xs
validHcl _ = False

validEcl ecl = ecl `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

validPid pid = length pid == 9 && case reads pid :: [(Int, String)] of
    [(_, "")] -> True
    _ -> False

validatePassport2 p = 
  vaildByr (p DM.! "byr")  && 
  vaildIyr (p DM.! "iyr") &&
  vaildEyr (p DM.! "eyr") &&
  validHgt (p DM.! "hgt") &&
  validHcl (p DM.! "hcl") &&
  validEcl (p DM.! "ecl") &&
  validPid (p DM.! "pid")

passportParser = do
  parts <- sepEndBy1 pp (char ' ' <|> char '\n')
  let passport = DM.fromList $ parts
  return $ if validatePassport passport && validatePassport2 passport then Just passport else Nothing

parseText = sepBy1 (passportParser) (string "\n")

parseFile = parse (parseText <* eof) "(unknown)"
parseFile' = parse parseText "(unknown)"

pass1 = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm"
pass2 = "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\nhcl:#cfa07d byr:1929"
pass3 = "hcl:#ae17e1 iyr:2013\neyr:2024\necl:brn pid:760753108 byr:1931\nhgt:179cm"
pass4 = "hcl:#cfa07d eyr:2025 pid:166559648\niyr:2011 ecl:brn hgt:59in"

main :: IO ()
main = do
  -- f <- readFile "sample"
  -- f <- readFile "sample2"
  -- f <- readFile "sample3"
  f <- readFile "input"

  let parsed = fromRight [] $ parseFile' f
  
  let valid = filter isJust parsed
  -- mapM_ print valid
  print valid
  print $ length valid

  putStrLn "End"