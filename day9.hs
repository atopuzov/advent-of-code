import Data.Either (fromRight)
import Data.Maybe (fromMaybe)
import Text.ParserCombinators.Parsec
import qualified Data.Map.Strict as DM

parseText' = sepBy nums (char ',') <* optional (char '\n')

sign = do
  char '-'
  return negate

nums = do
  sign <- option id sign
  n <- many1 digit
  return $ sign (read n :: Int)
parseFile = parse (parseText' <* eof) "(unknown)"

data MemAccess = Position | Immediate | Relative deriving Show
numToMode 0 = Position
numToMode 1 = Immediate
numToMode 2 = Relative
numToMode _ = undefined

decodeInstruction num = (de, c, b, a)
  where
    de = num `mod` 100
    c  = numToMode (num `div` 100   `mod` 10)
    b  = numToMode (num `div` 1000  `mod` 10)
    a  = numToMode (num `div` 10000 `mod` 10)

runProg input xs = runProg' input [] 0 0 xs
runProg' i o pc bc mem =
  if DM.null mem
  then []
  else case o' of
         []     ->    runProg' i' o' pc' bc' mem'
         (x:[]) -> x:(runProg' i' [] pc' bc' mem')
  where
    (i', o', pc', bc', mem') = stepProg i o pc bc mem

-- Parameters that an instruction writes to will never be in immediate mode.
stepProg input output i r xs = runOpCode opCode
  where
    instruction = readMem i
    (opCode, par1m, par2m, par3m) = decodeInstruction instruction
    par1  = readMem (i + 1)
    par2  = readMem (i + 2)
    par3  = readMem (i + 3)

    getArg Position  par = readMem par1
    getArg Immediate par = par
    getArg Relative  par = readMem (r + par)

    arg1   = getArg par1m par1
    arg2   = getArg par2m par2

    runOpCode 99 = (input,  output,  0,              r,  DM.empty)          -- End
    runOpCode 1  = (input,  output,  (i + 4),        r,  addMul)            -- Add
    runOpCode 2  = (input,  output,  (i + 4),        r,  addMul)            -- Multiply
    runOpCode 3  = (input', output,  (i + 2),        r,  storeInput)        -- Read input and store
    runOpCode 4  = (input,  output', (i + 2),        r,  xs)                -- Write to output
    runOpCode 5  = (input,  output,  (nextJmp (/=)), r,  xs)                -- Jump if true
    runOpCode 6  = (input,  output,  (nextJmp (==)), r,  xs)                -- Jump if false
    runOpCode 7  = (input,  output,  (i + 4),        r,  (setOneZero (<)))  -- Less then
    runOpCode 8  = (input,  output,  (i + 4),        r,  (setOneZero (==))) -- Equals
    runOpCode 9  = (input,  output,  (i + 2),        r', xs)

    r' = r + arg1

    operation 1 = (+)
    operation 2 = (*)
    operation _ = undefined

    nextJmp f = if arg1 `f` 0 then arg2 else (i+3)

    readMem addr | addr >= 0 = fromMaybe 0 (DM.lookup addr xs)
    readMem _                = error "Negative address read"
    updateMem addr cont | addr >= 0 = DM.insert addr cont xs
    updateMem _ _                   = error "Negative address write"

    setDest Position par = par
    setDest Relative par = r + par
    dst1 = setDest par1m par1
    dst3 = setDest par3m par3

    addMul       = updateMem dst3 (operation opCode arg1 arg2)
    storeInput   = updateMem dst1 inputValue
    setOneZero f = updateMem dst3 (if arg1 `f` arg2 then 1 else 0)

    inputValue = head input
    input' = tail input
    output' = [arg1]


main :: IO ()
main = do
  f <- readFile "input.txt"
  let parsed = fromRight [] $ parseFile f

  let xs = DM.fromList $ zip [0..] parsed
  putStrLn "Part1:"
  print $ runProg [1] xs

  putStrLn "Part2:"
  print $ runProg [2] xs
