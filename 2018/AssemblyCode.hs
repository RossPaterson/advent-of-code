module AssemblyCode where

import Parser
import Utilities
import Control.Applicative
import Data.Bits
import Data.Char
import Data.Map (Map, (!))
import qualified Data.Map as Map

-- Instructions

data GenInstruction a = Instruction a Int Int Int
  deriving Show

instance Functor GenInstruction where
    fmap f (Instruction i a b c) = Instruction (f i) a b c

type Instruction = GenInstruction OpCode
data OpCode =
    ADDR | ADDI | MULR | MULI | BANR | BANI | BORR | BORI |
    SETR | SETI | GTIR | GTRI | GTRR | EQIR | EQRI | EQRR
  deriving (Show, Eq, Ord, Bounded, Enum)

-- parse a string containing a single instruction
parseInstruction :: String -> Instruction
parseInstruction = runParser (instruction opcode)
  where
    opcode = foldr1 (<|>) [c <$ string (map toLower (show c)) | c <- allValues]

-- parser for a general instruction
instruction :: Parser a -> Parser (GenInstruction a)
instruction opcode =
    Instruction <$> opcode <* char ' ' <*> nat
        <* char ' ' <*> nat <* char ' ' <*> nat

type State = Map Int Int

-- initial state, with nr registers, each containing 0
initState :: Int -> State
initState nr = Map.fromList [(r, 0) | r <- [0..nr-1]]

-- execute a single instruction on the state
execute :: State -> Instruction -> State
execute s (Instruction ADDR a b c) = Map.insert c (s!a + s!b) s
execute s (Instruction ADDI a b c) = Map.insert c (s!a + b) s
execute s (Instruction MULR a b c) = Map.insert c (s!a * s!b) s
execute s (Instruction MULI a b c) = Map.insert c (s!a * b) s
execute s (Instruction BANR a b c) = Map.insert c (s!a .&. s!b) s
execute s (Instruction BANI a b c) = Map.insert c (s!a .&. b) s
execute s (Instruction BORR a b c) = Map.insert c (s!a .|. s!b) s
execute s (Instruction BORI a b c) = Map.insert c (s!a .|. b) s
execute s (Instruction SETR a _ c) = Map.insert c (s!a) s
execute s (Instruction SETI a _ c) = Map.insert c a s
execute s (Instruction GTIR a b c) = Map.insert c (fromEnum (a > s!b)) s
execute s (Instruction GTRI a b c) = Map.insert c (fromEnum (s!a > b)) s
execute s (Instruction GTRR a b c) = Map.insert c (fromEnum (s!a > s!b)) s
execute s (Instruction EQIR a b c) = Map.insert c (fromEnum (a == s!b)) s
execute s (Instruction EQRI a b c) = Map.insert c (fromEnum (s!a == b)) s
execute s (Instruction EQRR a b c) = Map.insert c (fromEnum (s!a == s!b)) s

-- Machine with one register designated as the instruction pointer

data Program = Program { ip :: Int, instructions :: Map Int Instruction }
  deriving Show

-- parse a program designating a register as instruction pointer
parseProgram :: String -> Program
parseProgram s =
    Program (runParser set_ip (head ls))
        (Map.fromList (zip [0..] (map parseInstruction (tail ls))))
  where
    set_ip = string "#ip " *> nat
    ls = lines s

-- run code from initial state to completion
finalState :: Program -> Int -> State
finalState c numRegisters =
    until (finished c) (step c) (initState numRegisters)

-- intermediate states when running code from initial state to completion
allStates :: Program -> Int -> [State]
allStates c numRegisters =
    takeWhile (not . finished c) $ iterate (step c) (initState numRegisters)

-- frequency of execution of each instruction in a sample run
heatMap :: Program -> Int -> Int -> [(Int, Int)]
heatMap c numRegisters maxSteps =
    frequency $ map (! ip c) $ take maxSteps $ allStates c numRegisters

-- a single fetch-execute step
step :: Program -> State -> State
step c s = incr c (execute s (instructions c!pc))
  where
    pc = s!ip c

-- increment the instruction pointer
incr :: Program -> State -> State
incr c s = Map.adjust (+1) (ip c) s

-- stop if the instruction pointer is outside the range of the code
finished :: Program -> State -> Bool
finished c s = not (Map.member (s!ip c) (instructions c))
