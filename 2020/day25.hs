import Text.ParserCombinators.Parsec

parseText = do
    card <- read <$> many1 digit <* newline
    door <- read <$> many1 digit <* newline
    return (card :: Int, door :: Int)
parseFile = parse (parseText <* spaces <* eof) "(unknown)"

doLoop sub val = (val * sub) `rem` 20201227

findLoop sub publicKey = length loops
    where
        loops = takeWhile (/=publicKey) $ iterate (doLoop sub) 1

findEncryptionNumber sub loops = iterate (doLoop sub) 1 !! loops

sol1 card door = findEncryptionNumber door (findLoop 7 card)

main :: IO ()
main = do
    -- f <- readFile "sample"
    f <- readFile "input"
    let Right (card, door) = parseFile f

    putStr "Task1: "
    print $ sol1 card door