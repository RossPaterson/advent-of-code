module Main where

import Parser
import Primes
import Utilities
import Control.Applicative
import Data.Map (Map, (!))
import qualified Data.Map as Map

-- Input processing

type Input = Code

data Code = Code { ip :: Int, instructions :: Map Int Instruction }
  deriving Show
data Instruction = Instruction OpCode Int Int Int
  deriving Show
data OpCode = ADDI | ADDR | EQRR | GTRR | MULI | MULR | SETI | SETR
  deriving Show

parse :: String -> Input
parse s =
    Code (runParser ip (head ls))
        (Map.fromList (zip [0..] (map (runParser instruction) (tail ls))))
  where
    ip = string "#ip " *> nat
    instruction =
        Instruction <$> opcode <* char ' ' <*> nat
            <* char ' ' <*> nat <* char ' ' <*> nat
    opcode =
        ADDI <$ string "addi" <|> ADDR <$ string "addr" <|>
        EQRR <$ string "eqrr" <|> GTRR <$ string "gtrr" <|>
        MULI <$ string "muli" <|> MULR <$ string "mulr" <|>
        SETI <$ string "seti" <|> SETR <$ string "setr"
    ls = lines s

-- Part One

solve1 :: Input -> Int
solve1 c = run c (initState 6) ! 0

-- run code from initial state to completion
run :: Code -> State -> State
run c = until (finished c) (step c)

type State = Map Int Int

-- a single fetch-execute step
step :: Code -> State -> State
step c s = incr c (execute (instructions c!pc) s)
  where
    pc = s!ip c

-- execute one instruction
execute :: Instruction -> State -> State
execute (Instruction ADDR a b c) s = Map.insert c (s!a + s!b) s
execute (Instruction ADDI a b c) s = Map.insert c (s!a + b) s
execute (Instruction EQRR a b c) s = Map.insert c (fromEnum (s!a == s!b)) s
execute (Instruction GTRR a b c) s = Map.insert c (fromEnum (s!a > s!b)) s
execute (Instruction MULI a b c) s = Map.insert c (s!a * b) s
execute (Instruction MULR a b c) s = Map.insert c (s!a * s!b) s
execute (Instruction SETI a b c) s = Map.insert c a s
execute (Instruction SETR a b c) s = Map.insert c (s!a) s

-- increment the instruction pointer
incr :: Code -> State -> State
incr c s = Map.adjust (+1) (ip c) s

-- stop if the instruction pointer is outside the range of the code
finished :: Code -> State -> Bool
finished c s = not (Map.member (s!ip c) (instructions c))

-- initially, all registers hold 0
initState :: Int -> State
initState nreg = Map.fromList [(r, 0) | r <- [0..nreg-1]]

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
solve2 = sumOfFactors . inputNumber 1

-- The input number when R0 initially contains v, which will be
-- the contents of R5 when control first reaches instruction 1.
inputNumber :: Int -> Code -> Int
inputNumber v c = (head $ dropWhile initializing $ iterate (step c) s0) ! 5
  where
    s0 = Map.insert 0 v (initState 6)
    initializing s = s!ip c /= 1

main :: IO ()
main = do
    s <- readFile "input19.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    print (solve2 input)
