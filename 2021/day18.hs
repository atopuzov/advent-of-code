import Text.ParserCombinators.Parsec
import Data.Either ( fromRight, rights )

data Fish a = PNum a | PPair (Fish a) (Fish a)
  deriving Eq

instance Show a => Show (Fish a) where
  show (PNum a) = show a
  show (PPair l r) = "[" ++ show l ++ "," ++ show r ++ "]"

parseFishNum :: Parser (Fish Int)
parseFishNum = do
  n <- many1 digit
  pure $ PNum (read n :: Int)

parseFishPair :: Parser (Fish Int)
parseFishPair = do
  char '['
  x <- parseFishPair <|> parseFishNum
  char ','
  y <- parseFishPair <|> parseFishNum
  char ']'
  pure $ PPair x y

parseText :: Parser [Fish Int]
parseText = sepEndBy parseFishPair (char '\n')

parseFile :: String -> Either ParseError [Fish Int]
parseFile = parse (parseText <* eof) "(unknown)"

addSF :: Fish a -> Fish a -> Fish a
addSF = PPair

splitSF :: Integral a => Fish a -> Maybe (Fish a)
splitSF (PNum v)
  | v >= 10 = Just $ PPair (PNum $ v `div` 2) (PNum $ (v + 1) `div` 2)
  | otherwise = Nothing
splitSF (PPair l r) = case splitSF l of
  Just l' -> Just $ PPair l' r
  Nothing -> case splitSF r of
    Just r' -> Just $ PPair l r'
    Nothing -> Nothing

reduceSF :: Integral a => Fish a -> Fish a
reduceSF f = case explodeSF f of
  Just f' -> reduceSF f'
  Nothing -> maybe f reduceSF (splitSF f)

explodeSF :: Num a => Fish a -> Maybe (Fish a)
explodeSF f = (\(x, _, _) -> x) <$> explode' 0 f
  where
    explode' 4 (PPair (PNum l) (PNum r)) = Just (PNum 0, l, r)
    explode' depth (PPair l r) =
      case explode' (depth + 1) l of
        Just (l', lln, lrn) -> Just (PPair l' (addToLeftmost lrn r), lln, 0)
        _ -> case explode' (depth + 1) r of
          Just (r', rln, rrn) -> Just (PPair (addToRightmost rln l) r', 0, rrn)
          _ -> Nothing
    explode' depth _ = Nothing

    addToLeftmost  n (PNum v)    = PNum (n+v)
    addToLeftmost  n (PPair l r) = PPair (addToLeftmost n l) r
    addToRightmost n (PNum v)    = PNum (n+v)
    addToRightmost n (PPair l r) = PPair l (addToRightmost n r)

magnitudeSF :: Fish Int -> Int
magnitudeSF (PNum v) = v
magnitudeSF (PPair l r) = 3 * magnitudeSF l + 2 * magnitudeSF r

task1 :: [Fish Int] -> Int
task1 fishes = magnitudeSF $ foldl1 ((reduceSF .) . addSF) fishes

task2 :: [Fish Int] -> Int
task2 fishes = maximum [compute a b | a <- fishes, b <- fishes, a /= b]
  where
    compute = ((magnitudeSF . reduceSF) .) . addSF

main :: IO ()
main = do
  f <- readFile "input"
  let i = fromRight [] $ parseFile f

  putStrLn "Task1:"
  print $ task1 i

  putStrLn "Task2:"
  print $ task2 i

