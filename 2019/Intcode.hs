-- toy computer for Days 2, 5, ...
module Intcode where

import Utilities
import Data.List
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Maybe

type Memory = Map Address Value
type Address = Int
type Value = Int

-- comma-separated list of values for locations 0..
readMemory :: String -> Memory
readMemory s = Map.fromAscList $ zip [0..] $ 
    map read (words [if c == ',' then ' ' else c | c <- s])

data State = State {
    memory :: Memory,
    curr_ip :: Address,
    inputs :: [Value], -- unread input values
    outputs :: [Value] -- output values in reverse order
    }
    deriving Show

initState :: [Value] -> Memory -> State
initState vs mem = State {
    memory = mem,
    curr_ip = 0,
    inputs = vs,
    outputs = []
    }

data Action
    = Set Address Value
    | ReadTo Address
    | Write Value
    | SetIP Value
    | Nop
    deriving Show

apply :: Action -> State -> State
apply (Set addr v) s = s { memory = Map.insert addr v (memory s) }
apply (Write v) s = s { outputs = v:outputs s }
apply (ReadTo addr) s = case (inputs s) of
    v:vs -> s { memory = Map.insert addr v (memory s), inputs = vs }
    [] -> error "no input"
apply (SetIP addr) s = s { curr_ip = addr }
apply Nop s = s

data Instruction
    = Add Parameter Parameter Address -- Day 2
    | Mul Parameter Parameter Address -- Day 2
    | Input Address -- Day 5 part 1
    | Output Parameter -- Day 5 part 1
    | JumpIfTrue Parameter Parameter -- Day 5 part 2
    | JumpIfFalse Parameter Parameter -- Day 5 part 2
    | LessThan Parameter Parameter Address -- Day 5 part 2
    | Equals Parameter Parameter Address -- Day 5 part 2
    | Halt -- Day 2
    deriving Show

-- instruction parameter
data Parameter = Position Address | Immediate Value
    deriving Show

-- returns decoded instruction and location of following instruction
fetch :: State -> (Instruction, Address)
fetch s = case leadvalue `mod` 100 of
    1 -> (Add (parameter 1) (parameter 2) (arg 3), ip+4)
    2 -> (Mul (parameter 1) (parameter 2) (arg 3), ip+4)
    3 -> (Input (arg 1), ip+2)
    4 -> (Output (parameter 1), ip+2)
    5 -> (JumpIfTrue (parameter 1) (parameter 2), ip+3)
    6 -> (JumpIfFalse (parameter 1) (parameter 2), ip+3)
    7 -> (LessThan (parameter 1) (parameter 2) (arg 3), ip+4)
    8 -> (Equals (parameter 1) (parameter 2) (arg 3), ip+4)
    99 -> (Halt, ip+1)
    n -> exec_error $ "Bad opcode " ++ show n
  where
    mem = memory s
    ip = curr_ip s
    leadvalue = mem_lookup "out of range" ip
    parameter n = mode n (arg n)
    arg n = mem_lookup ("parameter " ++ show n ++ " out of range") (ip+n)
    mode n v = case leadvalue `div` (10^(n+1)) `mod` 10 of
        0 -> Position v
        1 -> Immediate v
        d -> exec_error $ "bad parameter mode " ++ show d
    mem_lookup msg addr = fromMaybe (exec_error msg) (Map.lookup addr mem)
    exec_error msg = error (show ip ++ ": " ++ msg)

effect :: Instruction -> Memory -> Maybe Action
effect instr mem = case instr of
    Add x y r -> Just (Set r (get x + get y))
    Mul x y r -> Just (Set r (get x * get y))
    Input r -> Just (ReadTo r)
    Output x -> Just (Write (get x))
    JumpIfTrue x y -> Just (if get x /= 0 then SetIP (get y) else Nop)
    JumpIfFalse x y -> Just (if get x == 0 then SetIP (get y) else Nop)
    LessThan x y r -> Just (Set r (fromEnum (get x < get y)))
    Equals x y r -> Just (Set r (fromEnum (get x == get y)))
    Halt -> Nothing
  where
    get (Position addr) =
        fromMaybe (error ("address " ++ show addr ++ " out of range"))
            (Map.lookup addr mem)
    get (Immediate n) = n

-- fetch-execute cycle
advance :: State -> Maybe State
advance s = do
    let (instr, next_ip) = fetch s
    mod <- effect instr (memory s)
    return $ apply mod (s { curr_ip = next_ip })

-- run the machine until it halts
run :: Memory -> Memory
run = fst . runIO []

runIO :: [Value] -> Memory -> (Memory, [Value])
runIO vs mem = (memory s, reverse (outputs s))
  where
    s = whileJust advance (initState vs mem)

-- debugging

showState :: State -> String
showState s = unwords (map show f) ++ "." ++ unwords (map show b)
  where
    (f, b) = splitAt (curr_ip s) (Map.elems (memory s))

trace :: Memory -> [(Address, Instruction, Action)]
trace = traceIO []

traceIO :: [Value] -> Memory -> [(Address, Instruction, Action)]
traceIO vs mem = unfoldr advanceTrace (initState vs mem)

advanceTrace :: State -> Maybe ((Address, Instruction, Action), State)
advanceTrace s = do
    let (instr, next_ip) = fetch s
    mod <- effect instr (memory s)
    return $ ((curr_ip s, instr, mod), apply mod ( s { curr_ip = next_ip }))

-- Run the program with several inputs and return those for which it
-- produces different outputs than the function.
testProgram :: Memory -> (Value -> Value) -> [Value] -> [Value]
testProgram mem f xs = [x | x <- xs, snd (runIO [x] mem) /= [f x]]
