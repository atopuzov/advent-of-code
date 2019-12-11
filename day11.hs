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

    getArg Position  par = readMem par
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

createMemory xs = DM.fromList $ zip [0..] xs

data Turn = TLeft | TRight deriving Show
decodeDirection 0 = TLeft
decodeDirection 1 = TRight
data Facing = FUp | FDown | FLeft | FRight deriving (Show, Eq)
num2Facing 90  = FUp
num2Facing 180 = FLeft
num2Facing 270 = FDown
num2Facing 0   = FRight
facing2Num FUp    = 90
facing2Num FLeft  = 180
facing2Num FDown  = 270
facing2Num FRight = 0
turn2num TLeft  = 90
turn2num TRight = (-90)

calcturn facing turn = num2Facing $ (facing2Num facing + turn2num turn) `mod` 360
stepPoint FUp    (x,y) = (x,y-1)
stepPoint FDown  (x,y) = (x,y+1)
stepPoint FLeft  (x,y) = (x-1,y)
stepPoint FRight (x,y) = (x+1,y)

chunk [] = []
chunk as = (a,b):chunk rest
  where
    (a:b:[]) = take 2 as
    rest = drop 2 as

paint mem pixel = last states
  where
    getColor (canvas, point, _) = fromMaybe 0 $ DM.lookup point canvas
    states = scanl stepPaint (DM.singleton (0,0) pixel, (0,0), FUp) $ chunk outputs
    inputs = fmap getColor states
    outputs = runProg inputs mem

-- data PainterState = PainterState (Int,Int) (DM.Map Int Int)
stepPaint (canvas, point, facing) (color, turn) = (canvas', point', facing')
  where
    facing' = calcturn facing $ decodeDirection turn
    point' = stepPoint facing' point
    canvas' = DM.insert point color canvas

bounds dm = ([xmin..xmax], [ymin..ymax])
  where
    k = DM.keys dm
    xs = fmap fst k
    ys = fmap snd k
    xmin = minimum xs
    xmax = maximum xs
    ymin = minimum ys
    ymax = maximum ys

printImage dm = unlines image
  where
    printPixel pixel = case DM.lookup pixel dm of
      Just 0  -> ' ' -- Black
      Just 1  -> '*' -- White
      Nothing -> ' ' -- Not painted, was black
    (xrange, yrange) = bounds dm
    image = [ [ printPixel (x, y) | x <- xrange ] | y <- yrange ]

main :: IO ()
main = do
  f <- readFile "input.txt"
  let parsed = fromRight [] $ parseFile f

  let xs = createMemory parsed
  putStrLn "Part1:"
  let (canvas, _, _) = paint xs 0
  print $ length $ DM.keys canvas

  putStrLn "Part2:"
  let (canvas, _, _) = paint xs 1
  putStrLn $ printImage canvas
