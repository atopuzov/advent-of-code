import Text.ParserCombinators.Parsec
import Data.Either (fromRight)
import Data.Bits
import qualified Data.Map as DM
import qualified Data.List as DL

data Inst = Mask String | Mem Int Int deriving Show

maskParser = do
  string "mask = "
  Mask <$> many1 alphaNum

memParser = do
  string "mem["
  loc <- many1 digit
  string "] = "
  value <- many1 digit
  return $ Mem (read loc) (read value)

parseText = sepBy1 (try maskParser <|> try memParser) (char '\n')
parseFile = parse (parseText <* eof) "(unknown)"

calcMask mask = \value -> value .&. complement zeroMask .|. oneMask
  where
    maskBits = zip (reverse mask) [0..]
    (zeroMask, oneMask) = foldr f (0, 0) maskBits
    f ('0', b) (z, o) = (z + 2^b, o)
    f ('1', b) (z, o) = (z      , 2^b + o) 
    f _ a = a

write _    mem [] = mem
write _    mem (Mask mask:rest)      = write mask' mem rest
  where mask' = calcMask mask
write mask mem ((Mem addr val):rest) = write mask  mem' rest
  where mem' = DM.insert addr (mask val) mem

--  0 -> unchanged
--  1 -> set to 1
--  X -> both 1 and 0
addrs mask = \addr -> [ addr .&. complement clearMask .|. oneMask .|. f | f <- floating]
  where
    maskBits = zip (reverse mask) [0..]
    floating = sum . fmap (2 ^) <$> DL.subsequences xMask
    clearMask = last floating
    (oneMask, xMask) = foldr f (0, []) maskBits
    f ('1', b) (o, x) = (o + 2^b, x)
    f ('X', b) (o, x) = (o      , b:x) 
    f _ a = a

write' _ mem [] = mem
write' _ mem (Mask mask:rest) = write' mask' mem rest
  where mask' = addrs mask
write' mask mem ((Mem addr val):rest) = write' mask mem'' rest
  where
    mem' = DM.fromList [(addr', val) | addr' <- mask addr]
    mem'' = DM.union mem' mem

k :: Int -> [Int]
k _ = []
main = do
    -- f <- readFile "sample"
    -- f <- readFile "sample2"
    f <- readFile "input"

    let parsed = fromRight [] $ parseFile f

    putStr "Task1:"
    let mem = write id DM.empty parsed
    print $ sum $ DM.elems mem

    putStr "Task2:"
    let mem' = write' k DM.empty parsed
    print $ sum $ DM.elems mem'