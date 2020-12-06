import Data.Either (fromRight)
import Data.Maybe (fromMaybe)
import Text.ParserCombinators.Parsec
import qualified Data.Map.Strict as DM
import qualified Data.List as DL
import Data.Maybe as MB

parseText' = sepBy nums (char ',') <* optional (char '\n')
sign = do
  char '-'
  return negate
nums = do
  sign <- option id sign
  n <- many1 digit
  return $ sign $ read n
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

---------------------------------------------------------------------------
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
      Just 0   -> '█' -- '#'
      Just 1   -> '.'
      Just 2   -> 'O'
      _        -> ' '
    (xrange, yrange) = bounds dm
    image = [ [ printPixel (x, y) | x <- xrange ] | y <- yrange ]
---------------------------------------------------------------------------

data Out = Wall | Movable | Oxygen deriving (Show, Enum)
data Movement = North | South | West | East deriving (Show, Enum)
toMovement n   = toEnum (n-1) :: Movement
toOut n = toEnum n :: Out


move (x,y) 1 = (x, y-1) -- north
move (x,y) 2 = (x, y+1) -- south
move (x,y) 3 = (x-1,y)  -- west
move (x,y) 4 = (x+1,y)  -- east

--------------------------
takeWhileOneMore p = foldr (\x ys -> if p x then x:ys else [x]) []

stepBack 1 = 2
stepBack 2 = 1
stepBack 3 = 4
stepBack 4 = 3

explore    mem = runRobot mem (takeWhile (/=0)) id
findOxygen mem = runRobot mem id (takeWhileOneMore (/=2))

runRobot mem inputFilter outputFilter = last states
  where
    oldPos = (0, 0)
    map = DM.singleton oldPos 1
    states = DL.scanl' stepRobot (map, oldPos, [], 1, False) $ outputs
    inputs = inputFilter $ fmap (\(_,_,_,o,_) -> o) states
    outputs = outputFilter $ runProg inputs mem

stepRobot (world, oldPos, path, dir, backTrack) o =
  (world', newPos, path'', dir', backTrack')
  where
    exploredPos = move oldPos dir
    moved = not (o == 0)
    newPos = if moved
             then exploredPos
             else oldPos
    path' = if moved
            then if backTrack
                 then path
                 else dir:path
            else path
    path'' = if backTrack'
             then tail path'
             else path'
    backTrack' = MB.isJust north &&
                MB.isJust south &&
                MB.isJust west &&
                MB.isJust east
    north = DM.lookup (move newPos 1) world'
    south = DM.lookup (move newPos 2) world'
    west  = DM.lookup (move newPos 3) world'
    east  = DM.lookup (move newPos 4) world'
    dir'  = case (north, south, west, east) of
              (Nothing, _, _, _) -> 1
              (_, Nothing, _, _) -> 2
              (_, _, Nothing, _) -> 3
              (_, _, _, Nothing) -> 4
              _ -> case path' of
                     [h]    -> 0
                     (h:_) -> stepBack h
    world' = DM.insert exploredPos o world

stepFrom dm val = case upd of
                    [] -> dm
                    _  -> stepFrom dm' (val+1)
  where
    viable = DM.keys $ DM.filter (==val) dm
    locs = concat $ fmap (\x -> fmap (move x) [1..4]) viable
    vals = fmap (\x -> dm DM.! x) locs
    upd = filter ((==1) . snd) $ zip locs vals
    new = fmap (fmap (const (val+1))) upd
    dm' = DM.fromList new `DM.union` dm

main :: IO ()
main = do
  f <- readFile "input.txt"
  let parsed = fromRight [] $ parseFile f
  let xs = createMemory parsed

  putStrLn "Part1:"
  let a@(game, _, path, _, _) = findOxygen xs
  let path' = reverse path
  -- putStrLn $ printImage game
  -- print $ fmap toMovement $ path'

  print $ length path'

  putStrLn "Part2:"
  let a@(exploredMaze, _, path, _, _) = explore xs
  -- putStrLn $ printImage exploredMaze
  let oxidized = stepFrom exploredMaze 2
  print $ (maximum $ DM.elems oxidized) - 2
