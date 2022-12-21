module Main where

import Utilities
import Parser
import Control.Applicative
import Data.Map (Map, (!))
import qualified Data.Map as Map

-- Input processing

type Input = Program
type Program = Map String Node

data Node = Constant Int | Binary VarName Op VarName
    deriving (Show)
type VarName = String
data Op = Plus | Minus | Times | Divide
    deriving (Show)

parse :: String -> Input
parse = Map.fromList . map (runParser named_node) . lines
  where
    named_node = (,) <$> var_name <* string ": " <*> node
    node =
        Constant <$> nat <|>
        Binary <$> var_name <* space <*> op <* space <*> var_name
    op =
         Plus <$ char '+' <|>
         Minus <$ char '-' <|>
         Times <$ char '*' <|>
         Divide <$ char '/'
    var_name = some letter

-- Part One

-- evaluate a variable in the program
eval :: VarName -> Program -> Int
eval v node_map = value_map ! v
  where
    value_map = Map.map eval_node node_map
    eval_node (Constant n) = n
    eval_node (Binary v1 op v2) = evalOp op (value_map ! v1) (value_map ! v2)

evalOp :: Op -> Int -> Int -> Int
evalOp Plus x y = x + y
evalOp Minus x y = x - y
evalOp Times x y = x * y
evalOp Divide x y = x `div` y

solve1 :: Input -> Int
solve1 = eval "root"

testInput :: String
testInput = "\
    \root: pppw + sjmn\n\
    \dbpl: 5\n\
    \cczh: sllz + lgvd\n\
    \zczc: 2\n\
    \ptdq: humn - dvpt\n\
    \dvpt: 3\n\
    \lfqf: 4\n\
    \humn: 5\n\
    \ljgn: 2\n\
    \sjmn: drzm * dbpl\n\
    \sllz: 4\n\
    \pppw: cczh / lfqf\n\
    \lgvd: ljgn * ptdq\n\
    \drzm: hmdt - zczc\n\
    \hmdt: 32\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 152)]

-- Part Two

data Equation = Eq Expr Expr
    deriving (Show)

data Expr = VarExpr | ConstExpr Int | BinaryExpr Op Expr Expr
    deriving (Show)

-- More readable presentation of an equation

showEquation :: Equation -> String
showEquation (Eq e1 e2) = (showsExpr e1 . showString " = " . showsExpr e2) ""

showsExpr :: Expr -> ShowS
showsExpr VarExpr = showChar 'x'
showsExpr (ConstExpr n) = shows n
showsExpr (BinaryExpr op e1 e2) =
    showsParenExpr e1 . showsOp op . showsParenExpr e2

showsParenExpr :: Expr -> ShowS
showsParenExpr e@(BinaryExpr _ _ _) =
    showChar '(' . showsExpr e . showChar ')'
showsParenExpr e = showsExpr e

showsOp :: Op -> ShowS
showsOp Plus = showChar '+'
showsOp Minus = showChar '-'
showsOp Times = showChar '*'
showsOp Divide = showChar '/'

-- New interpretation of the program as an equation

equation :: Program -> Equation
equation node_map = case node_map ! "root" of
    Constant _ -> error "constant root"
    Binary v1 _ v2 -> Eq (expr node_map v1) (expr node_map v2)

expr :: Program -> VarName -> Expr
expr node_map v
  | v == "humn" = VarExpr
  | otherwise = nodeExpr node_map (node_map ! v)

nodeExpr :: Program -> Node -> Expr
nodeExpr _ (Constant n) = ConstExpr n
nodeExpr node_map (Binary v1 op v2) =
    BinaryExpr op (expr node_map v1) (expr node_map v2)

-- Simplify expression by folding constant subexpressions

simplify :: Equation -> Equation
simplify (Eq e1 e2) = Eq (simplifyExpr e1) (simplifyExpr e2)

simplifyExpr :: Expr -> Expr
simplifyExpr VarExpr = VarExpr
simplifyExpr (ConstExpr n) = ConstExpr n
simplifyExpr (BinaryExpr op e1 e2) =
    simplifyOp op (simplifyExpr e1) (simplifyExpr e2)

simplifyOp :: Op -> Expr -> Expr -> Expr
simplifyOp op (ConstExpr a) (ConstExpr b) = ConstExpr (evalOp op a b)
simplifyOp op e1 e2 = BinaryExpr op e1 e2

-- Solve an equation where one side is a constant and the other contains
-- a single occurrence of the variable, by inverting the expression.

solve :: Equation -> Int
solve (Eq (ConstExpr n) e) = invert e n
solve (Eq e (ConstExpr n)) = invert e n
solve _ = error "neither side is a constant"

invert :: Expr -> Int -> Int
invert VarExpr v = v
invert (BinaryExpr Plus (ConstExpr n) e) v = invert e (v - n)
invert (BinaryExpr Plus e (ConstExpr n)) v = invert e (v - n)
invert (BinaryExpr Minus (ConstExpr n) e) v = invert e (n - v)
invert (BinaryExpr Minus e (ConstExpr n)) v = invert e (n + v)
invert (BinaryExpr Times (ConstExpr n) e) v = invert e (v `div` n)
invert (BinaryExpr Times e (ConstExpr n)) v = invert e (v `div` n)
invert (BinaryExpr Divide (ConstExpr n) e) v = invert e (n `div` v)
invert (BinaryExpr Divide e (ConstExpr n)) v = invert e (n * v)
invert _ _ = error "expression is not invertible"

solve2 :: Input -> Int
solve2 = solve . simplify . equation

tests2 :: [(String, Int)]
tests2 = [(testInput, 301)]

main :: IO ()
main = do
    s <- readFile "input/21.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
