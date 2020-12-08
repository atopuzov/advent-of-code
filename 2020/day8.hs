{-# LANGUAGE RecordWildCards #-}

import Text.ParserCombinators.Parsec
    ( char, digit, string, eof, many1, sepBy1, (<|>), parse )
import Data.Either (fromRight)
import qualified Data.Map as DM
import Data.Monoid (First(First, getFirst))
import qualified Control.Monad.State as MS

data Computer = Computer {
  cMem :: DM.Map Int Instruction,
  cMemAccess :: DM.Map Int Int,
  cPc :: Int,
  cAcc :: Int} deriving Show

type ComputerState = MS.State Computer

data Instruction =
  Acc Int |
  Jmp Int |
  Nop Int deriving Show

signPos = do
  char '+'
  return id

signNeg = do
  char '-'
  return negate

nums = do
  sign <- signPos <|> signNeg
  n <- many1 digit
  return $ sign (read n :: Int)

acc = do
  string "acc "
  Acc <$> nums

jmp = do
  string "jmp "
  Jmp <$> nums

nop = do
  string "nop "
  Nop <$> nums

instructionParser = acc <|> jmp <|> nop

parseText = sepBy1 instructionParser (string "\n")

parseFile = parse (parseText <* eof) "(unknown)"

makeMem prog = DM.fromList $ zip [0..] prog

stepComputer =  do
  computer@Computer{..} <- MS.get

  let instruction = cMem DM.! cPc
  let pc' = newPc cPc instruction
  let acc' = newAcc cAcc instruction
  
  MS.put $ computer { cPc = pc', cAcc = acc'}
  where
    newPc pc (Jmp v) = pc + v
    newPc pc _       = pc + 1

    newAcc acc (Acc v) = acc + v
    newAcc acc _       = acc

verifyInfiniteLoopStepComputer = do
  computer@Computer{..} <- MS.get

  let count = DM.findWithDefault 0 cPc cMemAccess
  let count' = count + 1
  let access' = DM.insert cPc count' cMemAccess

  MS.put $ computer { cMemAccess = access'}

  if count' == 2
  then return False
  else do
    stepComputer
    return True

runComputer = do
  ok <- verifyInfiniteLoopStepComputer
  MS.when ok runComputer

mkComputer prog = 
  Computer { 
    cMem = makeMem prog, 
    cMemAccess = DM.empty,
    cPc = 0,
    cAcc = 0}

task1 prog = cAcc $ MS.execState runComputer (mkComputer prog)

bootCodeDone =  do
  Computer{..} <- MS.get
  return $ cPc == length cMem

runComputer' = do
  ok <- verifyInfiniteLoopStepComputer
  bootComplete <- bootCodeDone
  if ok && not bootComplete
  then runComputer'
  else if bootComplete
       then return True
       else return False

task2 prog = if boot then Just . cAcc $ computer else Nothing
  where
    (boot, computer) = MS.runState runComputer' (mkComputer prog)

mut = mut' []

flipInst (Jmp x) = Nop x
flipInst (Nop x) = Jmp x

mut' before (inst@(Acc _):after) = mut' (inst:before) after
mut' before (inst:after) = modified:rest
  where
    inst' = flipInst inst
    modified = foldr (:) after (reverse (inst':before))
    rest = mut' (inst:before) after
mut' before [] = [reverse before]


main :: IO ()
main = do
  -- f <- readFile "sample"
  f <- readFile "input"

  let parsed = fromRight [] $ parseFile f

  putStr "Task1: "
  print $ task1 parsed

  putStr "Task2: "
  let mutated = mut parsed
  print $ getFirst $ foldMap (First . task2) mutated
