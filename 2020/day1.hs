import qualified Data.Set as DS
import Data.Monoid ( First(First, getFirst) )
import Data.Maybe (maybe)

readInt :: String -> Int
readInt = read

isOk target ds num = 
        if diff `DS.member` ds'
        then Just num
        else Nothing
    where
        ds' = DS.delete num ds
        diff = target - num

sol1 num = num * (2020 - num)

task1 numbers = maybe 0 sol1 num
    where
        ds = DS.fromList numbers
        num = getFirst $ foldMap (First . isOk 2020 ds) ds


sol2 (x, y) = x * y * (2020 - x - y)

task2 numbers = maybe 0 sol2 num
    where
        ds = DS.fromList numbers
        num = getFirst $ foldMap (First . isOk2 ds) ds

isOk2 ds num = num2 >>= isOk'
    where
        isOk' x =
            if x `DS.member` ds'
            then Just (num, x)
            else Nothing
        ds' = DS.delete num ds
        target2 = 2020 - num
        num2 = getFirst $ foldMap (First . isOk target2 ds) ds'
   

main :: IO ()
main = do
    numbers <- fmap (fmap readInt  . lines) $ readFile "input"  

    putStr "Task1: "
    print $ task1 numbers

    putStr "Task2: "
    print $ task2 numbers