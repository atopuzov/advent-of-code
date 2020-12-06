-- import Data.Array
import Data.Either (fromRight)
import Text.ParserCombinators.Parsec
import qualified Data.Vector as DV
import Debug.Trace

parseText' = sepBy nums (char ',') <* optional (char '\n')

sign = do
  char '-'
  return negate

nums = do
  sign <- option id sign
  n <- many1 digit
  return $ sign (read n :: Int)
parseFile = parse (parseText' <* eof) "(unknown)"

decodeInstruction num = (de, c, b, a)
  where
    de = num `mod` 100
    c  = (num `div` 100   `mod` 10) == 1
    b  = (num `div` 1000  `mod` 10) == 1
    a  = (num `div` 10000 `mod` 10) == 1

runProg input xs = runProg' [input] [] 0 xs

-- Parameters that an instruction writes to will never be in immediate mode.
runProg' input output i xs = runOpCode opCode
  where
    instruction = xs DV.! i
    (opCode, arg1im, arg2im, arg3im) = decodeInstruction instruction
    par1  = xs DV.! (i + 1)
    par2  = xs DV.! (i + 2)
    par3  = xs DV.! (i + 3)
    arg1   = if arg1im then par1 else (xs DV.! par1)
    arg2   = if arg2im then par2 else (xs DV.! par2)

    runOpCode 99 = output
    runOpCode 1  = runProg' input  output  (i + 4)        addMul            -- Add
    runOpCode 2  = runProg' input  output  (i + 4)        addMul            -- Multiply
    runOpCode 3  = runProg' input' output  (i + 2)        storeInput        -- Read input and store
    runOpCode 4  = runProg' input  output' (i + 2)        xs                -- Write to output
    runOpCode 5  = runProg' input  output  (nextJmp (/=)) xs                -- Jump if true
    runOpCode 6  = runProg' input  output  (nextJmp (==)) xs                -- Jump if false
    runOpCode 7  = runProg' input  output  (i + 4)        (setOneZero (<))  -- Less then
    runOpCode 8  = runProg' input  output  (i + 4)        (setOneZero (==)) -- Equals

    operation 1 = (+)
    operation 2 = (*)
    operation _ = undefined

    nextJmp f = if arg1 `f` 0 then arg2 else (i+3)
    addMul  = xs DV.// [(par3, (operation opCode) arg1 arg2)]
    storeInput = xs DV.// [(par1, inputValue)]
    setOneZero f = xs DV.// [(par3, if arg1 `f` arg2 then 1 else 0)]

    inputValue = head input
    input' = tail input
    output' = [arg1]


main :: IO ()
main = do
  f <- readFile "input.txt"
  --f <- readFile "sample.txt"
  let parsed = fromRight [] $ parseFile f
  let xs = DV.fromList parsed
  putStrLn "Part1:"
  print $ head $ runProg 1 xs
  putStrLn "Part2:"
  print $ head $ runProg 5 xs
