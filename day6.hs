import Data.Either (fromRight)
import Text.ParserCombinators.Parsec
import qualified Data.Map as DM
import qualified Control.Applicative as CA

-- Parsing
eol = char '\n'
parseText = sepEndBy line eol

object = do
  i <- many1 (letter <|> digit)
  return i

line = do
  a <- object
  char ')'
  b <- object
  return (a, b)

parseFile = parse (parseText <* eof) "(unknown)"

-- AoC Challenge
buildTree vertices = foldr (\(k, v) dm -> DM.insertWith (++) k [v] dm) DM.empty vertices

nodePaths tree = depth' [] tree "COM"
  where
    depth' path tree node =
      case (DM.lookup node tree) of
        Nothing -> currentNode
        Just children -> foldr (DM.union) currentNode (depths children)
      where
        currentNode = DM.singleton node path
        depths cs = fmap (depth' (node:path) tree) cs

commonPath p1 p2 =  foldr (\(a, b) acc -> if a == b then a:acc else acc) [] $ zip p1 p2

orbitalTransfers orbits1 orbits2 intersection =
  CA.liftA2 (+) (CA.liftA2 (-) orbits1 intersection)
                (CA.liftA2 (-) orbits2 intersection)

sol1 tree = sum $ fmap length $ DM.elems tree
sol2 tree = orbitalTransfers (fmap length path1) (fmap length path2) (fmap length common)
  where
    path1 = fmap reverse $ DM.lookup "YOU" tree
    path2 = fmap reverse $ DM.lookup "SAN" tree
    common = commonPath <$> path1 <*> path2

main :: IO ()
main = do
  f <- readFile "input.txt"
  --f <- readFile "sample.txt"
  --f <- readFile "sample2.txt"
  let parsed = fromRight [] $ parseFile f
  let paths =  nodePaths $ buildTree parsed
  putStrLn "Part1:"
  print $ sol1 paths
  putStrLn "Part2:"
  print $ sol2 paths
