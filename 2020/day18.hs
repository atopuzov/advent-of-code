import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr

data Expr = Add Expr Expr |
            Mul Expr Expr |
            Val Int deriving Show

expr = buildExpressionParser part2 factor 

-- Part 1
part1 = [ [Infix (between spaces spaces (char '+') >> return Add) AssocLeft,
           Infix (between spaces spaces (char '*') >> return Mul) AssocLeft] ]

-- Part 2
part2 = [ [Infix (between spaces spaces (char '+') >> return Add) AssocLeft],
          [Infix (between spaces spaces (char '*') >> return Mul) AssocLeft] ]
 
valueParser = 
  between spaces spaces (Val . read <$> many1 digit)
openParen = between spaces spaces $ char '('
closeParen = between spaces spaces $ char ')'
exprParser = between openParen closeParen expr
factor = exprParser <|> valueParser

parseExprs = sepEndBy1 expr newline 
parseFile = parse (parseExprs <* eof) "(unknown)"

eval (Val x) = x
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

main :: IO ()
main = do
    f <- readFile "inputF"

    let evaluatedExpressions = fmap eval . parse expr "" <$> lines f
    putStrLn "Task1/2: "
    print $ sum <$> sequence evaluatedExpressions