import qualified Data.Set as DS
import qualified Data.List as DL

sumOf2 numbers num = or $ fmap sumsUp numbers
    where
        ds = DS.fromList numbers
        sumsUp x = (num - x) `DS.member` (DS.delete x ds)

verifyNumbers preamble numbers = verifyNumbers' [] start rest
    where
        start = reverse $ take preamble numbers
        rest = drop preamble numbers

verifyNumbers' acc prev (x:rest) = 
    if ok
    then verifyNumbers' (ok:acc) prev' rest
    else x
    where
        ok = sumOf2 prev x
        prev' = x:(init prev) 

-- will error if solution is not found
tst tgt xs = go 0 1 
    where 
        go i j
            | s == tgt = minimum nums + maximum nums
            | s < tgt  = go i     (j+1)
            | s > tgt  = go (i+1) 0
                where
                    s = sum nums
                    nums = take j $ drop i xs

main = do
    f <- readFile "sample"
    f <- readFile "input"
    let preamble = 25
    
    let numbers = fmap (read :: String -> Int) (lines f)

    let weak = verifyNumbers preamble numbers
    putStr "Task1: "
    print $ weak
    putStr "Task2: "
    print $ tst weak numbers
