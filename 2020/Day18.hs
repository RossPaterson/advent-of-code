module Main where

import Utilities
import Parser
import Control.Applicative

-- Expressions

type Input = [Expr]

-- abstract syntax of expressions
data Expr = Number Int | Binary Expr Op Expr
    deriving Show

data Op = Plus | Times
    deriving Show

value :: Expr -> Int
value (Number n) = n
value (Binary e1 Plus e2) = value e1 + value e2
value (Binary e1 Times e2) = value e1 * value e2

-- Part One

-- expressions, with + and * left associative and of equal precedence
parse1 :: String -> Expr
parse1 = runParser expr
  where
    expr =
        foldl (uncurry . Binary) <$> factor <*>
             many ((,) <$ space <*> op <* space <*> factor)
    op = Plus <$ char '+' <|> Times <$ char '*'
    factor = Number <$> nat <|> char '(' *> expr <* char ')'

solve1 :: String -> Int
solve1 = sum . map (value . parse1) . lines

tests1 :: [(String, Int)]
tests1 = [
    ("1 + 2 * 3 + 4 * 5 + 6", 71),
    ("1 + (2 * 3) + (4 * (5 + 6))", 51),
    ("2 * 3 + (4 * 5)", 26),
    ("5 + (8 * 3 + 9 + 3 * 4 * 3)", 437),
    ("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))", 12240),
    ("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2", 13632)]

-- Part Two

-- expressions, with + and * left associative and + binding more tightly than *
parse2 :: String -> Expr
parse2 = runParser expr
  where
    expr = foldl mul <$> term <*> many (string " * " *> term)
    mul e1 e2 = Binary e1 Times e2
    term = foldl add <$> factor <*> many (string " + " *> factor)
    add e1 e2 = Binary e1 Plus e2
    factor = Number <$> nat <|> char '(' *> expr <* char ')'

solve2 :: String -> Int
solve2 = sum . map (value . parse2) . lines

tests2 :: [(String, Int)]
tests2 = [
    ("1 + 2 * 3 + 4 * 5 + 6", 231),
    ("1 + (2 * 3) + (4 * (5 + 6))", 51),
    ("2 * 3 + (4 * 5)", 46),
    ("5 + (8 * 3 + 9 + 3 * 4 * 3)", 1445),
    ("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))", 669060),
    ("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2", 23340)]

main :: IO ()
main = do
    s <- readFile "input/18.txt"
    putStr (unlines (failures "solve1" (value . parse1) tests1))
    print (solve1 s)
    putStr (unlines (failures "solve2" (value . parse2) tests2))
    print (solve2 s)
