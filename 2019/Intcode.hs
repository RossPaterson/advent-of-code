-- toy computer for Days 2, 5, ...
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

data State = State {
    curr_ip :: Addr,
    memory :: Memory,
    inputs :: [Int], -- unread input values
    outputs :: [Int] -- output values in reverse order
    }
    deriving Show

initState :: [Int] -> Memory -> State
initState vs mem = State {
    curr_ip = 0,
    memory = mem,
    inputs = vs,
    outputs = []
    }

data Modification
    = Set Addr Int
    | ReadTo Addr
    | Write Int
    | SetIP Int
    | Nop
    deriving Show

apply :: Modification -> State -> State
apply (Set addr v) s = s { memory = Map.insert addr v (memory s) }
apply (Write v) s = s { outputs = v:outputs s }
apply (ReadTo addr) s = case (inputs s) of
    v:vs -> s { memory = Map.insert addr v (memory s), inputs = vs }
    [] -> error "no input"
apply (SetIP addr) s = s { curr_ip = addr }
apply Nop s = s

data Instruction
    = Add Parameter Parameter Addr -- Day 2
    | Mul Parameter Parameter Addr -- Day 2
    | Input Addr -- Day 5 part 1
    | Output Parameter -- Day 5 part 1
    | JumpIfTrue Parameter Parameter -- Day 5 part 2
    | JumpIfFalse Parameter Parameter -- Day 5 part 2
    | LessThan Parameter Parameter Addr -- Day 5 part 2
    | Equals Parameter Parameter Addr -- Day 5 part 2
    | Halt -- Day 2
    deriving Show

data Parameter = Parameter ParameterMode Int
    deriving Show

-- addressing modes for parameters
data ParameterMode = Position | Immediate
    deriving (Enum, Show)

-- returns decoded instruction and location of following instruction
fetch :: State -> (Instruction, Addr)
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
    parameter n = Parameter (mode n) (arg n)
    arg n = mem_lookup ("parameter " ++ show n ++ " out of range") (ip+n)
    mode n = toEnum (leadvalue `div` (10^(n+1)) `mod` 10)
    mem_lookup msg addr = fromMaybe (exec_error msg) (Map.lookup addr mem)
    exec_error msg = error (show ip ++ ": " ++ msg)

effect :: Instruction -> Memory -> Maybe Modification
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
    get (Parameter Position addr) =
        fromMaybe (error ("address " ++ show addr ++ " out of range"))
            (Map.lookup addr mem)
    get (Parameter Immediate n) = n

-- fetch-execute cycle
advance :: State -> Maybe State
advance s = do
    let (instr, next_ip) = fetch s
    mod <- effect instr (memory s)
    return $ apply mod (s { curr_ip = next_ip })

-- run the machine until it halts
run :: Memory -> Memory
run = fst . runIO []

runIO :: [Int] -> Memory -> (Memory, [Int])
runIO vs mem = (memory s, reverse (outputs s))
  where
    s = whileJust advance (initState vs mem)

-- debugging

showState :: State -> String
showState s = unwords (map show f) ++ "." ++ unwords (map show b)
  where
    (f, b) = splitAt (curr_ip s) (Map.elems (memory s))

trace :: Memory -> [(Int, Instruction, Modification)]
trace = traceIO []

traceIO :: [Int] -> Memory -> [(Int, Instruction, Modification)]
traceIO vs mem = unfoldr advanceTrace (initState vs mem)

advanceTrace :: State -> Maybe ((Int, Instruction, Modification), State)
advanceTrace s = do
    let (instr, next_ip) = fetch s
    mod <- effect instr (memory s)
    return $ ((curr_ip s, instr, mod), apply mod ( s { curr_ip = next_ip }))

-- Run the program with several inputs and return those for which it
-- produces different outputs than the function.
testProgram :: Memory -> (Int -> Int) -> [Int] -> [Int]
testProgram mem f xs = [x | x <- xs, snd (runIO [x] mem) /= [f x]]
