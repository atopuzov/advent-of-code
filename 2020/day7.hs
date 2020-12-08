import Text.ParserCombinators.Parsec
import Data.Either (fromRight)
import Data.Maybe ( fromJust )
import qualified Data.Graph as DG
import Data.Foldable ( Foldable(toList) )
import qualified Data.Map as DM

bagDesc = do
  a <- many1 letter
  char ' '
  b <- many1 letter
  return $ a ++ " " ++ b

emptyBag = do
  string "no other bags"
  return []

nonEmpybag' = do
    n <- many1 digit
    char ' '
    bag <- bagDesc
    string " bag"
    optional $ char 's'
    return (read n :: Int, bag)
    -- return bag

notEmptyBag = sepBy1 (nonEmpybag') (string ", ")
bagContents = try emptyBag <|> notEmptyBag
ruleParser = do
  bag <- bagDesc
  string " bags contain "
  c <- bagContents
  char '.'
  return (bag, c)

parseText = sepBy1 ruleParser (string "\n")

parseFile = parse (parseText <* eof) "(unknown)"

createEdges (bag, bags) = (bag, bag, fmap snd bags)

calcBags bags bag = sum $ fmap subBags requiredBags
  where
    requiredBags = bags DM.! bag
    subBags (q, bag') = q + q * (calcBags bags bag')

main :: IO ()
main = do
  f <- readFile "sample"
  f <- readFile "input"

  let parsed = fromRight [] $ parseFile f
  -- mapM_ (putStrLn . show) parsed

  let edgeList = fmap createEdges parsed
  let (graph, nodeFromVertex, vertexFromKey) = DG.graphFromEdges edgeList
  let transposed = DG.transposeG graph
  let start = vertexFromKey "shiny gold"
  let reachable = (DG.reachable transposed) (fromJust start)

  putStr "Task1: "
  print $ (length reachable) - 1 

  putStr "Task2: "
  let bags = DM.fromList parsed
  print $ calcBags bags "shiny gold"