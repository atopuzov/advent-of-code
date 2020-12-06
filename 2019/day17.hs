import Data.Either (fromRight)
import Data.Maybe (fromMaybe)
import Text.ParserCombinators.Parsec
import qualified Data.Map.Strict as DM
import qualified Data.List as DL
import qualified Data.Maybe as MB
import qualified Data.Char as DC

parseText' = sepBy nums (char ',') <* optional (char '\n')
sign = do
  char '-'
  return negate
nums = do
  sign <- option id sign
  n <- many1 digit
  return $ sign $ (read n :: MemCell)
parseFile = parse (parseText' <* eof) "(unknown)"

data MemAccess = Position | Immediate | Relative deriving (Show, Enum)

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
    c  = toEnum (num `div` 100   `mod` 10) :: MemAccess
    b  = toEnum (num `div` 1000  `mod` 10) :: MemAccess
    a  = toEnum (num `div` 10000 `mod` 10) :: MemAccess

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
      []     -> []
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
      Just n   -> n
      _        -> ' '
    (xrange, yrange) = bounds dm
    image = [ [ printPixel (x, y) | x <- xrange ] | y <- yrange ]

loc y x a = ((x, y), a)
prepLine y line = zipWith (loc y) [0..] line
readImage = concat . zipWith prepLine [0..]

isVacuum '^' = True
isVacuum 'v' = True
isVacuum '<' = True
isVacuum '>' = True
isVacuum  _  = False
isScaffold '#' = True
isScaffold  _  = False

scaffoldingIntersections image = DM.keys scaffInter
  where
    scaffLocations = DM.filter (=='#') image
    scaffInter = DM.filterWithKey fun scaffLocations
    fun (x,y) a = case isIntersection <$> u <*> d <*> l <*> r of
      Nothing -> False
      Just n  -> n
      where
        u = DM.lookup (x,y-1) image
        d = DM.lookup (x,y+1) image
        l = DM.lookup (x-1,y) image
        r = DM.lookup (x+1,y) image
    isIntersection u d l r = and $ fmap isScaffold [u, d, l, r]

sol1 image = sum $ fmap (uncurry (*)) $ scaffoldingIntersections image

data Facing = North | East | South | West deriving (Show, Eq, Enum)
data Cmd = TurnRight | TurnLeft | Walk deriving (Show, Eq)

turnLeft North = West
turnLeft d = pred d
turnRight West = North
turnRight d = succ d

move (x,y) North = (x, y-1)
move (x,y) South = (x, y+1)
move (x,y) West  = (x-1,y)
move (x,y) East  = (x+1,y)

vacFacing '^' = North
vacFacing 'v' = South
vacFacing '<' = West
vacFacing '>' = East

compressInstructions [] = []
compressInstructions (TurnLeft:rest)  = "L":(compressInstructions rest)
compressInstructions (TurnRight:rest) = "R":(compressInstructions rest)
compressInstructions walks@(Walk:rest) = (show numWalks):(compressInstructions rest')
  where
    numWalks = length $ takeWhile (==Walk) walks
    rest' = dropWhile (==Walk) rest

vacuumLocation image = (loc, vacFacing facing)
  where
    ((loc, facing):[]) = DM.toList $ DM.filter isVacuum image

getVacuumPath image = compressInstructions path
  where
    path = getVacuumPath' vacLoc vacFac image
    (vacLoc,vacFac) = vacuumLocation image

getVacuumPath' loc dir image = if (not isLeft && not isRight && not isNext)
  then []
  else (command:(getVacuumPath' loc' dir' image))
  where
    next  = move loc dir
    left  = turnLeft dir
    right = turnRight dir
    isNext  = isScaffold' next
    isLeft  = isScaffold' $ move loc left
    isRight = isScaffold' $ move loc right
    isScaffold' l = isScaffold $ DM.findWithDefault ' ' l image

    loc' = if isNext then next else loc

    dir' = case (isNext, isLeft, isRight) of
      (True, _, _) -> dir
      (_, True, _) -> left
      (_, _, True) -> right
      _ -> error "We have a problem!"

    command = case (isNext, isLeft, isRight) of
      (True, _, _) -> Walk
      (_, True, _) -> TurnLeft
      (_, _, True) -> TurnRight
      _ -> error "We have a problem!"

main :: IO ()
main = do
  f <- readFile "input.txt"
  let parsed = fromRight [] $ parseFile f
  let xs = createMemory parsed
  let view = fmap DC.chr $ runProg [] xs

  let image' = DM.fromList $ readImage $ lines view

  putStrLn "Part1:"
  print $ sol1 image'

  let instructions = getVacuumPath image'
  print $ unwords instructions

  -- done by hand :-P
  let code = "A,A,B,C,C,A,B,C,A,B\n" ++
             "L,12,L,12,R,12\n" ++
             "L,8,L,8,R,12,L,8,L,8\n" ++
             "L,10,R,8,R,12\n" ++ "n\n"
  let input = fmap DC.ord code

  let xs' = DM.insert 0 2 xs
  let dusted = last $ runProg input xs'
  putStrLn "Part2:"
  print $ dusted
