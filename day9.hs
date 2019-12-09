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

data OpCode = OpAdd | OpMultiply | OpRead | OpWrite | OpJumpIfTrue
  | OpJumpIfFalse | OpLessThen | OpEquals | OpSetRB | OpHalt deriving Show
numToOpcode 1  = OpAdd
numToOpcode 2  = OpMultiply
numToOpcode 3  = OpRead
numToOpcode 4  = OpWrite
numToOpcode 5  = OpJumpIfTrue
numToOpcode 6  = OpJumpIfFalse
numToOpcode 7  = OpLessThen
numToOpcode 8  = OpEquals
numToOpcode 9  = OpSetRB
numToOpcode 99 = OpHalt

decodeInstruction num = (de, c, b, a)
  where
    de = numToOpcode (num `mod` 100)
    c  = numToMode (num `div` 100   `mod` 10)
    b  = numToMode (num `div` 1000  `mod` 10)
    a  = numToMode (num `div` 10000 `mod` 10)

type MachineState = (Int, Int, DM.Map Int Int)
data MStatus = MHalt | MStep MachineState |
  MOutput Int MachineState | MInput (Int -> MStatus)

runProg input xs = runProg' input 0 0 xs
runProg' i pc bc mem = case stepProg pc bc mem of
  MHalt -> []
  MStep (pc', bc', mem') -> runProg' i pc' bc' mem'
  MOutput x (pc', bc', mem') -> x:(runProg' i pc' bc' mem')
  MInput f -> runProg' (tail i) pc' bc' mem'
    where
      MStep (pc', bc', mem') = f (head i)

-- Parameters that an instruction writes to will never be in immediate mode.
stepProg i r xs =
  case opCode of
    OpAdd         ->               MStep        ((i + 4),        r,  addMul)
    OpMultiply    ->               MStep        ((i + 4),        r,  addMul)
    OpRead        -> MInput (\x -> MStep        ((i + 2),        r,  storeInput x))
    OpWrite       ->               MOutput arg1 ((i + 2),        r,  xs)
    OpJumpIfTrue  ->               MStep        ((nextJmp (/=)), r,  xs)
    OpJumpIfFalse ->               MStep        ((nextJmp (==)), r,  xs)
    OpLessThen    ->               MStep        ((i + 4),        r,  (setOneZero (<)))
    OpEquals      ->               MStep        ((i + 4),        r,  (setOneZero (==)))
    OpSetRB       ->               MStep        ((i + 2),        r', xs)
    OpHalt        ->               MHalt

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

    r' = r + arg1

    operation OpAdd = (+)
    operation OpMultiply = (*)
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

    addMul           = updateMem dst3 (operation opCode arg1 arg2)
    storeInput input = updateMem dst1 input
    setOneZero f     = updateMem dst3 (if arg1 `f` arg2 then 1 else 0)

main :: IO ()
main = do
  f <- readFile "input.txt"
  let parsed = fromRight [] $ parseFile f

  let xs = DM.fromList $ zip [0..] parsed
  putStrLn "Part1:"
  print $ runProg [1] xs

  putStrLn "Part2:"
  print $ runProg [2] xs
