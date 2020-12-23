import Text.ParserCombinators.Parsec
import qualified Data.Set as DS
import Data.Either (fromRight)

readInt :: String -> Int
readInt = read

parsePlayer = do
  string "Player "
  n <- readInt <$> many1 digit 
  string ":\n"
  cards <- sepEndBy1 (readInt <$> many1 digit) newline
  return (n, cards)

parseText = sepEndBy1 parsePlayer newline
parseFile = parse (parseText <* spaces <* eof) "(unknown)"

sol1 game = sum $ zipWith (*) (reverse winner) [1..]
  where
    winner = play (player1, player2) 
    player1 = snd $ game !! 0
    player2 = snd $ game !! 1

    play ([], p2) = p2
    play (p1, []) = p1
    play (p1t:p1r, p2t:p2r)
      | p1t > p2t = play (p1r ++ [p1t, p2t], p2r)
      | p2t > p1t = play (p1r, p2r ++ [p2t, p1t])

sol2 game = calcPlayer p1 + calcPlayer p2
  where
    (p1, p2) = play2 (player1, player2) DS.empty
    calcPlayer p = sum $ zipWith (*) (reverse p) [1..]
    player1 = snd $ game !! 0
    player2 = snd $ game !! 1

    play2 game@(_, [])  _ = game
    play2 game@([], _)  _ = game
    play2 game@(p1, _) played
      | alreadyPlayed      = (p1, [])
      | otherwise          = play2' game played'
      where
        played' = DS.insert game played
        alreadyPlayed = DS.member game played

    play2' (p1t:p1r, p2t:p2r) played 
      | p1left >= p1t && p2left >= p2t = case newGame of
        (_, []) -> play2 p1won played
        ([], _) -> play2 p2won played
      | p1t > p2t = play2 p1won played
      | p2t > p1t = play2 p2won played
      where
        newGame = play2 (take p1t p1r, take p2t p2r) DS.empty
        p1won = (p1r ++ [p1t, p2t], p2r)
        p2won = (p1r              , p2r ++ [p2t, p1t])
        p1left = length p1r
        p2left = length p2r

main :: IO ()
main = do
    -- f <- readFile "sample"
    f <- readFile "input"

    let parsed = fromRight [] $ parseFile f
    
    putStr "Task1: "
    print $ sol1 parsed

    putStr "Task2: "
    print $ sol2 parsed 
