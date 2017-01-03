module Main where

import Parser
import Utilities
import Control.Applicative
import Data.Map (Map, (!))
import qualified Data.Map as Map

data Reg = A | B
  deriving (Show, Eq, Ord, Enum, Bounded)
type Offset = Int
data Instruction
    = HLF Reg
    | TPL Reg
    | INC Reg
    | JMP Offset
    | JIE Reg Offset
    | JIO Reg Offset
  deriving Show
type CodeAddr = Int
type Input = Map CodeAddr Instruction

parse :: String -> Input
parse = Map.fromList . zip [0..] . map (runParser instruction) . lines
  where
    instruction =
        HLF <$ string "hlf " <*> reg <|>
        TPL <$ string "tpl " <*> reg <|>
        INC <$ string "inc " <*> reg <|>
        JMP <$ string "jmp " <*> offset <|>
        JIE <$ string "jie " <*> reg <* string ", " <*> offset <|>
        JIO <$ string "jio " <*> reg <* string ", " <*> offset
    reg = A <$ char 'a' <|> B <$ char 'b'
    offset = negate <$ char '-' <*> nat <|> char '+' *> nat

type Registers = Map Reg Int
data State = State CodeAddr Registers
  deriving Show

zeroRegisters :: Registers
zeroRegisters = Map.fromList [(r, 0) | r <- allValues]

execute :: Instruction -> State -> State
execute (HLF r) (State pc regs) =
    State (pc+1) (Map.adjust (`div` 2) r regs)
execute (TPL r) (State pc regs) =
    State (pc+1) (Map.adjust (*3) r regs)
execute (INC r) (State pc regs) =
    State (pc+1) (Map.adjust (+1) r regs)
execute (JMP offset) (State pc regs) =
    State (pc+offset) regs
execute (JIE r offset) (State pc regs)
  | even (regs!r) = State (pc+offset) regs
  | otherwise = State (pc+1) regs
execute (JIO r offset) (State pc regs)
  | regs!r == 1 = State (pc+offset) regs
  | otherwise = State (pc+1) regs
 
run :: Input -> Registers -> Registers
run code regs0 = regs
  where
    State _ regs = whileJust step (State 0 regs0)
    step s@(State pc regs) = do
        instr <- Map.lookup pc code
        return (execute instr s)

solve1 :: Input -> Int
solve1 code = run code zeroRegisters ! B

test =
    "inc a\n\
    \jio a, +2\n\
    \tpl a\n\
    \inc a\n"

-- Part Two --

solve2 :: Input -> Int
solve2 code = run code (Map.insert A 1 zeroRegisters) ! B

main :: IO ()
main = do
    s <- readFile "input23.txt"
    let input = parse s
    print (solve1 input)
    print (solve2 input)
