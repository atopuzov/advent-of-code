import Text.ParserCombinators.Parsec
import Data.Either (fromRight)

instructionParser = do
    i <- letter
    n <- many1 digit
    return (i, read n :: Int)

parseText = sepBy1 instructionParser (string "\n")
parseFile = parse (parseText <* eof) "(unknown)"

--    N
--  W   E
--    S

toDegrees 'E' = 0
toDegrees 'N' = 90
toDegrees 'W' = 180
toDegrees 'S' = 270

toFacing 0   = 'E'
toFacing 90  = 'N'
toFacing 180 = 'W'
toFacing 270 = 'S'

step (ship@(x, y), f) (i, n)
    | i `elem` ['L', 'R']                = (ship, newFacing)
    | i `elem` ['N', 'E', 'W', 'S', 'F'] = (movedShip, f)
        where
            newFacing = toFacing $ (toDegrees f + turnDegrees) `mod` 360
            turnDegrees
                | i == 'L' = n
                | i == 'R' = -n
            movedShip
                | isDirection 'N' = (x,     y + n)
                | isDirection 'S' = (x,     y - n)
                | isDirection 'E' = (x + n, y)
                | isDirection 'W' = (x - n, y)
            isDirection direction = 
                (i == direction) || (i == 'F' && f == direction)

step2 (ship@(sx, sy), waypoint@(wx, wy)) (i, n)
    | i `elem` ['N', 'E', 'W', 'S'] = (ship, movedWaypoint)
    | i `elem` ['L', 'R']           = (ship, rotatedWaypoint)
    | i == 'F'                      = (movedShip, waypoint)
        where
            movedShip = (sx + n * wx, sy + n * wy)
            movedWaypoint 
                | i == 'N' = (wx,     wy + n)
                | i == 'S' = (wx,     wy - n)
                | i == 'E' = (wx + n, wy)
                | i == 'W' = (wx - n, wy)
            rotatedWaypoint 
                | i == 'L' = rotateWaypoint n 
                | i == 'R' = rotateWaypoint (360 - n)
            rotateWaypoint 0   = (wx, wy)
            rotateWaypoint 90  = (-wy, wx)
            rotateWaypoint 180 = (-wx, -wy)
            rotateWaypoint 270 = (wy, -wx)

main = do
    -- f <- readFile "sample"
    f <- readFile "input"

    let parsed = fromRight [] $ parseFile f
    -- print $ parsed

    let ship = (0, 0)
    let starting = (ship, 'E')
    let ((x, y), _) = foldl step starting parsed

    putStr "Task1: "
    print $ abs x + abs y

    let waypoint = (10, 1)
    let ((sx, sy), _) = foldl step2 (ship, waypoint) parsed

    putStr "Task2: "
    print $ abs sx + abs sy
