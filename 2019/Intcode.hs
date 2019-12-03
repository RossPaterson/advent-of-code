-- toy computer for Days 2, ...
module Intcode where

import Utilities
import Data.List
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Maybe

type Addr = Int
type Memory = Map Addr Int

-- comma-separated list of values for locations 0..
readMemory :: String -> Memory
readMemory s = Map.fromAscList $ zip [0..] $ 
    map read (words [if c == ',' then ' ' else c | c <- s])

data State = State { curr_ip :: Addr, memory :: Memory }
    deriving Show

data Modification
    = Set Addr Int
    | SetIP Int
    deriving Show

apply :: Modification -> State -> State
apply (Set addr v) (State ip mem) = State ip (Map.insert addr v mem)
apply (SetIP addr) (State _ mem) = State addr mem

data Instruction
    = Add Addr Addr Addr -- Day 2
    | Mul Addr Addr Addr -- Day 2
    | Halt -- Day 2
    deriving Show

-- returns decoded instruction and location of following instruction
fetch :: State -> (Instruction, Addr)
fetch (State ip mem) = case mem_lookup "out of range" ip of
    1 -> (Add (arg 1) (arg 2) (arg 3), ip+4)
    2 -> (Mul (arg 1) (arg 2) (arg 3), ip+4)
    99 -> (Halt, ip+1)
    n -> exec_error $ "Bad opcode " ++ show n
  where
    arg n = mem_lookup ("parameter " ++ show n ++ " out of range") (ip+n)
    mem_lookup msg addr = fromMaybe (exec_error msg) (Map.lookup addr mem)
    exec_error msg = error (show ip ++ ": " ++ msg)

effect :: Instruction -> Memory -> Maybe Modification
effect instr mem = case instr of
    Add x y r -> Just (Set r (get x + get y))
    Mul x y r -> Just (Set r (get x * get y))
    Halt -> Nothing
  where
    get addr =
        fromMaybe (error ("address " ++ show addr ++ " out of range"))
            (Map.lookup addr mem)

advance :: State -> Maybe State
advance s@(State ip mem) = do
    let (instr, next_ip) = fetch s
    mod <- effect instr mem
    return $ apply mod (State next_ip mem)

-- run the machine until it halts
run :: Memory -> Memory
run = memory . whileJust advance . State 0

output :: Memory -> Int
output mem = mem!0

-- debugging

showState :: State -> String
showState (State ip mem) = unwords (map show f) ++ "." ++ unwords (map show b)
  where
    (f, b) = splitAt ip (Map.elems mem)

trace :: Memory -> [(Int, Instruction, Modification)]
trace = unfoldr advanceTrace . State 0
  where
    advanceTrace s@(State ip mem) = do
        let (instr, next_ip) = fetch s
        mod <- effect instr mem
        return $ ((ip, instr, mod), apply mod (State next_ip mem))
