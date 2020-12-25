import Text.ParserCombinators.Parsec
import qualified Data.Map as DM
import qualified Data.Set as DS
import Data.Either (fromRight)

type Coordinate = Int
type Coordinates = (Coordinate, Coordinate, Coordinate)
type Floor = DM.Map Coordinates Bool

parseInstructions = 
    many1 $ choice [try (string "se"), try (string "sw"), try (string "e"),
                    try (string "nw"), try (string "ne"), try (string "w")]

parseText = sepEndBy1 parseInstructions newline
parseFile = parse (parseText <* spaces <* eof) "(unknown)"

--     NW NE 
--   W   X   E
--     SW SE
neighbourTile :: Coordinates -> String -> Coordinates
neighbourTile (x, y, z) "e"  = (x+1, y-1, z)
neighbourTile (x, y, z) "w"  = (x-1, y+1, z)
neighbourTile (x, y, z) "nw" = (x,   y+1, z-1)
neighbourTile (x, y, z) "ne" = (x+1, y,   z-1)
neighbourTile (x, y, z) "sw" = (x-1, y,   z+1)
neighbourTile (x, y, z) "se" = (x,   y-1, z+1)

flipTile floor tile = DM.alter flipT tile floor
    where
        flipT Nothing = Just True
        flipT v       = fmap not v

procLine :: Floor -> [String] -> Floor
procLine floor directions = flipTile floor endTile
    where
        endTile = foldr (flip neighbourTile) (0, 0, 0) directions

sol1 lines = length $ DM.keys blackTiles
    where
        blackTiles = DM.filter id floor'
        floor' = foldr (flip procLine) DM.empty lines

allNeighbours (x,y,z) = [
    (x+dx,y+dy,z+dz) | dx <- [-1,0,1], dy <- [-1,0,1], dz <- [-1,0,1], 
     dx + dz + dy == 0 && (dx,dy,dz) /= (0,0,0)]

evolveTile floor tile
    | isBlack && blackNeighbours == 0     = False
    | isBlack && blackNeighbours > 2      = False
    | not isBlack && blackNeighbours == 2 = True
    | otherwise                           = isBlack
    where
        isBlack = Just True == DM.lookup tile floor
        neighbouringTiles = allNeighbours tile
        neighbours = [n | x <- neighbouringTiles, 
                          let n = DM.findWithDefault False x floor, n]
        blackNeighbours = length neighbours      

evolve :: Floor -> Floor
evolve floor = newFloor
    where
        blackTiles = DM.keys floor
        tiles = concatMap allNeighbours blackTiles
        newFloor = DM.fromList [(tile, evolved) | tile <- tiles, let evolved = evolveTile floor tile, evolved]

sol2 lines = solution
    where
        solution = length $ DM.keys floor'
        floor = foldr (flip procLine) DM.empty lines
        floor' = foldr (const evolve) floor [1..100]

main :: IO ()
main = do
    -- f <- readFile "sample"
    f <- readFile "input"
    let parsed = fromRight [] $ parseFile f

    putStr "Task1: "
    print $ sol1 parsed

    putStr "Task2: "
    print $ sol2 parsed
