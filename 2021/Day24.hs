module Main where

import Utilities
import Data.List

-- Input processing

{-
The input consists of 14 blocks of the form

     0: inp w
     1: mul x 0
     2: add x z
     3: mod x 26
     4: div z A
     5: add x B
     6: eql x w
     7: eql x 0
     8: mul y 0
     9: add y 25
    10: mul y x
    11: add y 1
    12: mul z y
    13: mul y 0
    14: add y w
    15: add y C
    16: mul y x
    17: add z y

where A is either 1 or 26, with equal numbers of each, B is either 10
or more (when A is 1) or negative (when A is 26), and C is non-negative.

We see that z is never negative, and the above disassembles to

	if (z%26 + B == w)
		z = z/A
	else
		z = (z/A)*26 + w + C

When A is 1, this simplifies to

	z = z*26 + w + C

When A is 26, it simplifies to

	if (z%26 + B == w)
		z = z/A
	else
		z = (z/A)*26 + w + C

That is, z is being used as a stack, and these snippets are

	push(w + C)
and
	if (top() + B == w)
		pop()
	else
		pop()
		push(w + C)
respectively.

In order for the final z to be zero, the final stack must be empty, which
requires that each of these top() tests succeed, so the current digit must
be a specified offset from one seen earlier.
-}

data StackOp
    = PushPlus Int -- push the current digit plus n
    | PopPlus Int -- the current digit must equal pop() + n
    deriving Show

type Input = [StackOp]

parse :: String -> Input
parse = map stackOp . takes 18 . lines
  where
    -- extract the constants from an 18-instruction block
    stackOp block
      | a == 1 = PushPlus c
      | otherwise = PopPlus b
      where
        a = arg 4
        b = arg 5
        c = arg 15
        arg n = read (last (words (block!!n))) :: Int

-- Part One

-- Equation i j n means: d_i = d_j + n, with n >= 0
data Equation = Equation Pos Pos Int
    deriving Show
type Pos = Int

-- deduce relationships between digits from the stack operations
equations :: [StackOp] -> [Equation]
equations = eqn_aux [] . zip [1..]

eqn_aux :: [(Pos, Int)] -> [(Pos, StackOp)] -> [Equation]
eqn_aux _ [] = []
eqn_aux stk ((d, PushPlus n):ops) = eqn_aux ((d, n):stk) ops
eqn_aux ((d1, n1):stk) ((d2, PopPlus n2):ops) =
    equation d1 d2 (n1 + n2) : eqn_aux stk ops
eqn_aux [] _ = error "empty stack"

equation :: Pos -> Pos -> Int -> Equation
equation i j offset
  | offset >= 0 = Equation j i offset
  | otherwise = Equation i j (negate offset)

maxDigits :: Equation -> [(Pos, Int)]
maxDigits (Equation i j n) = [(i, 9), (j, 9-n)]

solve1 :: Input -> String
solve1 = concatMap show . map snd . sort . concatMap maxDigits . equations

-- Part Two

minDigits :: Equation -> [(Pos, Int)]
minDigits (Equation i j n) = [(i, 1+n), (j, 1)]

solve2 :: Input -> String
solve2 = concatMap show . map snd . sort . concatMap minDigits . equations

main :: IO ()
main = do
    s <- readFile "input/24.txt"
    let input = parse s
    putStrLn (solve1 input)
    putStrLn (solve2 input)
