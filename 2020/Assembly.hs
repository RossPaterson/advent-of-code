module Assembly (
    Program, Instruction(..), parseProgram,
    State (..), states, runProgram
    ) where

import Utilities
import Parser
import Control.Applicative
import Data.Map (Map)
import qualified Data.Map as Map

-- Input processing

type Program = Map Int Instruction

data Instruction
    = Acc Int
    | Jmp Int
    | Nop Int
    deriving Show

parseProgram :: String -> Program
parseProgram = Map.fromList . zip [0..] . map (runParser instruction) . lines
  where
    instruction =
        Acc <$ string "acc " <*> arg <|>
        Jmp <$ string "jmp " <*> arg <|>
        Nop <$ string "nop " <*> arg
    arg = char '+' *> nat <|> negate <$ char '-' <*> nat

data State = State {
    accumulator :: Int,
    pc :: Int
    }
    deriving Show

initState :: State
initState = State 0 0

-- execute a single instruction
action :: State -> Instruction -> State
action s (Acc n) = s { accumulator = accumulator s + n, pc = pc s + 1 }
action s (Jmp n) = s { pc = pc s + n }
action s (Nop _) = s { pc = pc s + 1 }

-- a single execution step
step :: Program -> State -> Maybe State
step p s = fmap (action s) $ Map.lookup (pc s) p

-- all states until the program terminates
states :: Program -> [State]
states p = iterateWhileJust (step p) initState

-- run the program to completion
runProgram :: Program -> State
runProgram p = whileJust (step p) initState
