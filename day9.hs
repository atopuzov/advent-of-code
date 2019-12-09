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
  return $ sign $ read n
parseFile = parse (parseText' <* eof) "(unknown)"

data MemAccess = Position | Immediate | Relative deriving Show
numToMode 0 = Position
numToMode 1 = Immediate
numToMode 2 = Relative
numToMode _ = error "Unknown access mode"

type MemCell = Int
data OpCode = OpBin (MemCell -> MemCell -> MemCell) | OpRead | OpWrite | OpJump (MemCell -> MemCell -> Bool)
  | OpSet (MemCell -> MemCell -> Bool) | OpSetRB | OpHalt
numToOpcode 1  = OpBin (+)
numToOpcode 2  = OpBin (*)
numToOpcode 3  = OpRead
numToOpcode 4  = OpWrite
numToOpcode 5  = OpJump (/=)
numToOpcode 6  = OpJump (==)
numToOpcode 7  = OpSet (<)
numToOpcode 8  = OpSet (==)
numToOpcode 9  = OpSetRB
numToOpcode 99 = OpHalt

decodeInstruction num = (de, c, b, a)
  where
    de = numToOpcode (num `mod` 100)
    c  = numToMode (num `div` 100   `mod` 10)
    b  = numToMode (num `div` 1000  `mod` 10)
    a  = numToMode (num `div` 10000 `mod` 10)

type MachineState = (MemCell, MemCell, DM.Map MemCell MemCell)
data MStatus = MHalt | MStep MachineState |
  MOutput MemCell MachineState | MInput (MemCell -> MStatus)

getMState (MStep state) = state
runProg input xs = runProg' input (0, 0, xs)
runProg' input state =
  case stepProg state of
    MHalt            -> []
    MStep state'     -> runProg' input state'
    MOutput x state' -> x:(runProg' input state')
    MInput f         -> case input of
      []     -> error "Empty input!"
      (x:xs) -> runProg' xs (getMState $ f x)

-- Parameters that an instruction writes to will never be in immediate mode.
stepProg (i, r, xs) =
  case opCode of
    OpBin f  ->                MStep        (i + 4,        r,  binOp f)
    OpRead   -> MInput $ \x -> MStep        (i + 2,        r,  storeInput x)
    OpWrite  ->                MOutput arg1 (i + 2,        r,  xs)
    OpJump f ->                MStep        (nextJmp f,    r,  xs)
    OpSet f  ->                MStep        (i + 4,        r,  setOneZero f)
    OpSetRB  ->                MStep        (i + 2,        r', xs)
    OpHalt   ->                MHalt

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

    nextJmp f = if arg1 `f` 0 then arg2 else i + 3

    readMem addr | addr >= 0 = fromMaybe 0 (DM.lookup addr xs)
    readMem _                = error "Negative address read"
    updateMem addr cont | addr >= 0 = DM.insert addr cont xs
    updateMem _ _                   = error "Negative address write"

    setDest Position par = par
    setDest Relative par = r + par

    dst1 = setDest par1m par1
    dst3 = setDest par3m par3

    binOp f          = updateMem dst3 (f arg1 arg2)
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
