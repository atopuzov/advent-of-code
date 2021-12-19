{-# LANGUAGE TupleSections #-}

import Data.Char (isLower, isUpper)
import Data.Either (fromRight)
import qualified Data.Map as DM
import qualified Data.Set as DS
import Text.ParserCombinators.Parsec
  ( ParseError,
    Parser,
    char,
    eof,
    letter,
    many1,
    parse,
    sepEndBy1,
  )

data Cave
  = StartCave
  | EndCave
  | SmallCave String
  | LargeCave String
  | BadCave String
  deriving (Ord, Eq)

instance Show Cave where
  show StartCave = "start"
  show EndCave = "end"
  show (SmallCave s) = s
  show (LargeCave s) = s
  show _ = error "Unable to show"

type CaveGraph = DM.Map Cave (DS.Set Cave)

cavesParser :: Parser [(Cave, DS.Set Cave) ]
cavesParser = do
  start <- caveType <$> many1 letter
  char '-'
  end <- caveType <$> many1 letter
  return [(start, DS.singleton end), (end, DS.singleton start)]
  where
    caveType "start" = StartCave
    caveType "end" = EndCave
    caveType s
      | all isLower s = SmallCave s
      | all isUpper s = LargeCave s
      | otherwise = BadCave s

parseText :: Parser CaveGraph
parseText = do
  caves <- sepEndBy1 cavesParser (char '\n')
  return $ DM.fromListWith DS.union $ concat caves

parseFile :: String -> Either ParseError CaveGraph
parseFile = parse (parseText <* eof) "(unknown)"

isSmallCave :: Cave -> Bool
isSmallCave (SmallCave _) = True
isSmallCave _ = False

exploreCave :: (Bool -> Bool) -> CaveGraph -> [Cave] -> Cave -> [[Cave]]
exploreCave c graph path cave =
  case cave of
    BadCave _ -> error "Nope can't do"
    EndCave -> [reverse path']
    StartCave -> paths'
    SmallCave _ -> paths'
    LargeCave _ -> paths'
  where
    path' = cave : path
    connections = DS.difference (graph DM.! cave) $ DS.singleton StartCave

    smallCavesInPath = filter isSmallCave path'
    smallCavesInPathCount = DM.fromListWith (+) $ fmap (,1) smallCavesInPath
    smallCaveCountedTwice = not . DM.null $ DM.filter (== 2) smallCavesInPathCount

    smallCavesAlreadyExplored =
      if c smallCaveCountedTwice
        then DM.keysSet smallCavesInPathCount
        else DM.keysSet $ DM.filter (/= 1) smallCavesInPathCount

    toExplore = DS.toList $ DS.difference connections smallCavesAlreadyExplored

    possblePaths = fmap (exploreCave c graph path') toExplore
    paths' = concat possblePaths

task1 :: CaveGraph -> Int
task1 graph = length $ exploreCave not graph [] StartCave

task2 :: CaveGraph -> Int
task2 graph = length $ exploreCave id graph [] StartCave

main :: IO ()
main = do
  f <- readFile "input"
  let i = fromRight DM.empty $ parseFile f

  putStrLn "Task1:"
  print $ task1 i

  putStrLn "Task2:"
  print $ task2 i
