import Text.ParserCombinators.Parsec
import qualified Data.Map as DM
import qualified Data.Set as DS
import qualified Data.List as DL
import Data.Either (fromRight)
import Data.Maybe (catMaybes, isJust, fromJust)
import Data.Tuple (swap)

parseAlergens = between (char '(') (char ')')
    (string "contains " >> 
    sepBy1 (many1 alphaNum) (string ", "))

parseIngredient = do
  i <- sepEndBy1 (many1 alphaNum) (char ' ')
  a <- parseAlergens
  return (i, a)

parseText = sepEndBy1 parseIngredient newline
parseFile = parse (parseText <* spaces <* eof) "(unknown)"

doFood []    acc  = acc
doFood ((foods, alergens):r) acc = doFood r (doOne foods alergens acc)

doOne _     []    acc = acc
doOne foods (a:r) acc = doOne foods r acc'
  where
    acc' = DM.insert a newFoods acc
    newFoods = case acc DM.!? a of
      Nothing -> DS.fromList foods
      Just x  -> DS.intersection x (DS.fromList foods)

getAllFoods = foldr f DS.empty
  where f x acc = foldr DS.insert acc (fst x)

getAllFoodsWithAlergens mp = foldr DS.union DS.empty (DM.elems mp)

sol1 foods = sum $ fmap (\x -> length . DS.toList $ DS.intersection (DS.fromList (fst x)) okFood) foods
  where
    okFood = DS.difference allFoods foodsWithAlergens
    alergens = doFood foods DM.empty
    foodsWithAlergens = getAllFoodsWithAlergens alergens
    allFoods = getAllFoods foods

sol2 foods = solveAlergens alergens DM.empty 
  where
    alergens = doFood foods DM.empty
    foodsWithAlergens = getAllFoodsWithAlergens alergens
    allFoods = getAllFoods foods    

getCanonicalIngredients alergensInIngredients = DL.intercalate "," ingredients
  where
    algToIng = DM.toList $ DM.map (head . DS.toList) alergensInIngredients
    ingredients = snd <$> DL.sort algToIng

solveAlergens alergens known =
  if DM.null alergens
  then getCanonicalIngredients known
  else solveAlergens alergens' known'
  where
    alergens' = DM.map (`DS.difference` uniqueAlergens) nonSolvedAlergens
    known' = DM.union known uniqueFoodToAlergensMap
    nonSolvedAlergens = DM.difference alergens uniqueFoodToAlergensMap
    uniqueAlergens = DS.fromList $ concatMap DS.toList $ DM.elems uniqueFoodToAlergensMap
    uniqueFoodToAlergensMap = DM.filter (\x -> length (DS.toList x) == 1) alergens


main :: IO ()
main = do
    -- f <- readFile "sample"
    f <- readFile "input"

    let parsed = fromRight [] $ parseFile f

    putStr "Task1: "
    print $ sol1 parsed

    putStr "Task1: "
    print $ sol2 parsed
