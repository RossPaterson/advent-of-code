module Main where

import AssemblyCode
import Numbers
import Utilities
import Data.Maybe
import Data.Map (Map, (!))
import qualified Data.Map as Map

-- Input processing

type Input = Program

parse :: String -> Input
parse = parseProgram

-- Part One

solve1 :: Input -> Int
solve1 p = finalState p numRegisters ! 0

numRegisters :: Int
numRegisters = 6

tests1 :: [(String, Int)]
tests1 = [(testInput, 7)]

testInput :: String
testInput = "\
\#ip 0\n\
\seti 5 0 1\n\
\seti 6 0 2\n\
\addi 0 1 0\n\
\addr 1 2 3\n\
\setr 1 0 0\n\
\seti 8 0 4\n\
\seti 9 0 5\n"

-- Part Two

{-
The input code disassembles to:

    f = <small number>;
    if (a != 0)
        f += <huge number>;
    a = 0;
    e = 1;
    do {
        c = 1;
        do {
            if (e*c == f)
                a += e;
            c++;
        } while (c <= f);
        e++;
    } while (e <= f);

That is, it computes the sum of the factors of f in f^2 iterations.

In the assembly code, the program starts by jumping to after the main
loop, does the initialization, and then jumps back to location 1 for
the main computation.
-}

solve2 :: Input -> Int
solve2 = sumOfFactors . target 1

-- The bound used in the first GTRR if R0 initially contains v.
target :: Int -> Program -> Int
target v p =
    head [s!b |
        s <- iterate (step p) (Map.insert 0 v (initState numRegisters)),
        Instruction GTRR a b c <-
            maybeToList (Map.lookup (s!ip p) (instructions p))]

main :: IO ()
main = do
    s <- readFile "input/19.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    print (solve2 input)
