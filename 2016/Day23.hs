module Main where

import Parser
import Utilities
import Control.Applicative
import Data.Map (Map, (!))
import qualified Data.Map as Map

data Reg = A | B | C | D
  deriving (Show, Eq, Ord, Enum, Bounded)
type Offset = Int
data Value = Value Int | Reg Reg
  deriving Show
data Instruction
    = Copy Value Value
    | Incr Value
    | Decr Value
    | JNZ Value Value
    | Toggle Value
  deriving Show
type CodeAddr = Int
type Code = Map CodeAddr Instruction
type Input = Code

parse :: String -> Input
parse = Map.fromList . zip [0..] . map (runParser instruction) . lines
  where
    instruction =
        Copy <$ string "cpy " <*> value <* char ' ' <*> value <|>
        Incr <$ string "inc " <*> value <|>
        Decr <$ string "dec " <*> value <|>
        JNZ <$ string "jnz " <*> value <* char ' ' <*> value <|>
        Toggle <$ string "tgl " <*> value
    value = Value <$> int <|> Reg <$> reg
    reg = A <$ char 'a' <|> B <$ char 'b' <|> C <$ char 'c' <|> D <$ char 'd'

toggle :: Instruction -> Instruction
toggle (Copy src dest) = JNZ src dest
toggle (Incr v) = Decr v
toggle (Decr v) = Incr v
toggle (JNZ val off) = Copy val off
toggle (Toggle off) = Incr off

type Registers = Map Reg Int
data State = State CodeAddr Registers Code
  deriving Show

value :: Value -> Registers -> Int
value (Value n) _ = n
value (Reg r) regs = regs!r

zeroRegisters :: Registers
zeroRegisters = Map.fromList [(r, 0) | r <- allValues]

initState :: Code -> State
initState code = State 0 (Map.insert A 7 zeroRegisters) code

fetch :: State -> Maybe Instruction
fetch (State pc regs code) = Map.lookup pc code

execute :: Instruction -> State -> State
execute (Copy val (Reg r)) (State pc regs code) =
    State (pc+1) (Map.insert r (value val regs) regs) code
execute (Incr (Reg r)) (State pc regs code) =
    State (pc+1) (Map.adjust (+1) r regs) code
execute (Decr (Reg r)) (State pc regs code) =
    State (pc+1) (Map.adjust (subtract 1) r regs) code
execute (JNZ val addr) (State pc regs code)
  | value val regs /= 0 = State (pc+offset) regs code
  | otherwise = State (pc+1) regs code
  where
    offset = value addr regs
execute (Toggle addr) (State pc regs code) =
    State (pc+1) regs (Map.adjust toggle (pc+offset) code)
  where
    offset = value addr regs
execute _ (State pc regs code) = State (pc+1) regs code

run :: State -> Registers
run s0 = regs
  where
    State _ regs _ = whileJust step s0
    step s = do
        instr <- fetch s
        return (execute instr s)

solve1 :: Input -> Int
solve1 code = run (initState code) ! A

run_code :: Input -> Int -> Registers
run_code code i = run (State 0 (Map.insert A i zeroRegisters) code)

test =
    "cpy 2 a\n\
    \tgl a\n\
    \tgl a\n\
    \tgl a\n\
    \cpy 1 a\n\
    \dec a\n\
    \dec a\n"

-- Part Two --

{-
Pseudocode of first 19 instructions:

    a := n
    FOR i := n-1 DOWNTO 1 DO
        a := a*i
        toggle instruction 16+2*i

When n >= 6, this exits the first loop with the code from 18 changed to:

18  cpy 1 c
19  cpy 73 c
20  cpy 80 d
21  inc a
22  dec d
23  jnz d -2
24  dec c
25  jnz c -5

which is equivalent to

    a := a + 73*80
-}

combination :: Int -> Int
combination n
  | n < 6 = error "loops forever"
  | otherwise = product [1..n] + 73*80

solve2 :: Input -> Int
solve2 code = combination 12

main :: IO ()
main = do
    s <- readFile "input23.txt"
    let input = parse s
    print (solve1 input)
    print (solve2 input)
