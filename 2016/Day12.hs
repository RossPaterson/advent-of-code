module Main where

import Parser
import Utilities
import Control.Applicative
import Data.Map (Map, (!))
import qualified Data.Map as Map

data Reg = A | B | C | D
  deriving (Show, Eq, Ord, Enum, Bounded)
type Offset = Int
data Value = Value Integer | Reg Reg
  deriving Show
data Instruction
    = Copy Value Reg
    | Incr Reg
    | Decr Reg
    | JNZ Value Offset
  deriving Show
type CodeAddr = Int
type Input = Map CodeAddr Instruction

parse :: String -> Input
parse = Map.fromList . zip [0..] . map (runParser instruction) . lines
  where
    instruction =
        Copy <$ string "cpy " <*> val <* char ' ' <*> reg <|>
        Incr <$ string "inc " <*> reg <|>
        Decr <$ string "dec " <*> reg <|>
        JNZ <$ string "jnz " <*> val <* char ' ' <*> int
    val = Value <$> int <|> Reg <$> reg
    reg = A <$ char 'a' <|> B <$ char 'b' <|> C <$ char 'c' <|> D <$ char 'd'

type Registers = Map Reg Integer
data State = State CodeAddr Registers
  deriving Show

value :: Value -> Registers -> Integer
value (Value n) _ = n
value (Reg r) rs = rs!r

zeroRegisters :: Registers
zeroRegisters = Map.fromList [(r, 0) | r <- allValues]

initState :: State
initState = State 0 zeroRegisters

execute :: Instruction -> State -> State
execute (Copy val r) (State pc regs) =
    State (pc+1) (Map.insert r (value val regs) regs)
execute (Incr r) (State pc regs) =
    State (pc+1) (Map.adjust (+1) r regs)
execute (Decr r) (State pc regs) =
    State (pc+1) (Map.adjust (subtract 1) r regs)
execute (JNZ val offset) (State pc regs)
  | value val regs /= 0 = State (pc+offset) regs
  | otherwise = State (pc+1) regs

run :: Input -> State -> Registers
run code s0 = regs
  where
    State _ regs = whileJust step s0
    step s@(State pc _) = do
        instr <- Map.lookup pc code
        return (execute instr s)

solve1 :: Input -> Integer
solve1 code = run code initState ! A

testInput :: String
testInput =
    "cpy 41 a\n\
    \inc a\n\
    \inc a\n\
    \dec a\n\
    \jnz a 2\n\
    \dec a\n"

tests1 :: [(String, Integer)]
tests1 = [(testInput, 42)]

-- Part Two --

initState2 :: State
initState2 = State 0 (Map.insert C 1 zeroRegisters)

solve2 :: Input -> Integer
solve2 code = run code initState2 ! A

main :: IO ()
main = do
    s <- readFile "input/12.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    print (solve2 input)
