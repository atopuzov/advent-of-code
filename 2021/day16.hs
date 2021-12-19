import Text.ParserCombinators.Parsec
import Data.Either ( fromRight )
import Data.Char

parseText :: Parser Packet
parseText = parsePacket <* many (char '0')

parseFile :: String -> Either ParseError Packet
parseFile = parse (parseText <* spaces ) "(unknown)"

-- All in parsec
parseBit :: Parser Int
parseBit = digitToInt <$> oneOf "01"

parsePacket = do
  packetVersion <- bin2dec <$> count 3 parseBit
  packetType    <- bin2dec <$>  count 3 parseBit
  case packetType of
    4 -> PLit packetVersion <$> parseLiteralValue
    0 -> PSum packetVersion <$> parseOperator
    1 -> PProd packetVersion <$> parseOperator
    2 -> PMin packetVersion <$> parseOperator
    3 -> PMax packetVersion <$> parseOperator
    5 -> PGT packetVersion <$> parseOperator
    6 -> PLT packetVersion <$> parseOperator
    7 -> PEQ packetVersion <$> parseOperator
    _ -> error $ "Unknown packet type!" ++ show packetType

parseLiteralValue :: Parser Int
parseLiteralValue = do
  groups <- many valueGroupContinue
  endGroup <- valueGroupEnd
  let num = bin2dec $ concat groups ++ endGroup
  pure num
  where
    valueGroupContinue = do
      char '1'
      count 4 parseBit
    valueGroupEnd = do
      char '0'
      count 4 parseBit

parseOperator :: Parser [Packet]
parseOperator = do
  i <- parseBit
  case i of
    0 -> parseOperator0
    _ -> parseOperator1

parseOperator0 :: Parser [Packet]
parseOperator0 = do
  l <- count 15 parseBit
  sub <- count (bin2dec l) (oneOf "01")
  case parse (many parsePacket) "(unknown)" sub of
    Right subpackets -> return subpackets
    Left e -> error (show e)

parseOperator1 :: Parser [Packet]
parseOperator1 = do
  n <- bin2dec <$> count 11 parseBit
  count n parsePacket

bin2dec :: [Int] -> Int
bin2dec = foldl (\acc a -> acc*2 + a) 0

hex2bin :: Char -> [Char]
hex2bin '0' = "0000"
hex2bin '1' = "0001"
hex2bin '2' = "0010"
hex2bin '3' = "0011"
hex2bin '4' = "0100"
hex2bin '5' = "0101"
hex2bin '6' = "0110"
hex2bin '7' = "0111"
hex2bin '8' = "1000"
hex2bin '9' = "1001"
hex2bin 'A' = "1010"
hex2bin 'B' = "1011"
hex2bin 'C' = "1100"
hex2bin 'D' = "1101"
hex2bin 'E' = "1110"
hex2bin 'F' = "1111"
hex2bin  _  = ""

data Packet =
    PLit Int Int
  | PSum Int [Packet]
  | PProd Int [Packet]
  | PMin Int [Packet]
  | PMax Int [Packet]
  | PGT Int [Packet]
  | PLT Int [Packet]
  | PEQ Int [Packet]
    deriving (Show)

packetVersionSum :: Packet -> Int
packetVersionSum (PLit v _)    = v
packetVersionSum (PSum v sub)  = v + sum (fmap packetVersionSum sub)
packetVersionSum (PProd v sub) = v + sum (fmap packetVersionSum sub)
packetVersionSum (PMin v sub)  = v + sum (fmap packetVersionSum sub)
packetVersionSum (PMax v sub)  = v + sum (fmap packetVersionSum sub)
packetVersionSum (PGT v sub)   = v + sum (fmap packetVersionSum sub)
packetVersionSum (PLT v sub)   = v + sum (fmap packetVersionSum sub)
packetVersionSum (PEQ v sub)   = v + sum (fmap packetVersionSum sub)

evalPacket :: Packet -> Int
evalPacket (PLit _ val)  = val
evalPacket (PSum _ sub)  = sum (fmap evalPacket sub)
evalPacket (PProd _ sub) = product (fmap evalPacket sub)
evalPacket (PMin _ sub)  = minimum (fmap evalPacket sub)
evalPacket (PMax _ sub)  = maximum (fmap evalPacket sub)
evalPacket (PGT _ sub)   = if evalPacket (head sub) >  evalPacket (sub !! 1) then 1 else 0
evalPacket (PLT _ sub)   = if evalPacket (head sub) <  evalPacket (sub !! 1) then 1 else 0
evalPacket (PEQ _ sub)   = if evalPacket (head sub) == evalPacket (sub !! 1) then 1 else 0


main :: IO ()
main = do
  f <- readFile "input"
  let i = concatMap hex2bin f
  let packet = parseFile i

  putStrLn "Task1:"
  print $ fmap packetVersionSum packet

  putStrLn "Task2:"
  print $ fmap evalPacket packet
