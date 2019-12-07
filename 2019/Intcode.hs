-- toy computer for Days 2, 5, 7, ...
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
    inputs :: [Value] -- unread input values
    }
    deriving Show

initState :: [Value] -> Memory -> State
initState vs mem = State {
    memory = mem,
    curr_ip = 0,
    inputs = vs
    }

data Action
    = Set Address Value
    | ReadTo Address
    | Write Value
    | SetIP Value
    | Nop
    deriving Show

-- an action may produce an output value, and can update the state
apply :: Action -> State -> (Maybe Value, State)
apply (Set addr v) s =
    (Nothing, s { memory = Map.insert addr v (memory s) })
apply (Write v) s = (Just v, s)
apply (ReadTo addr) s = case (inputs s) of
    v:vs -> (Nothing, s { memory = Map.insert addr v (memory s), inputs = vs })
    [] -> error "no input"
apply (SetIP addr) s = (Nothing, s { curr_ip = addr })
apply Nop s = (Nothing, s)

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
advance :: State -> Maybe (Maybe Value, State)
advance s = do
    let (instr, next_ip) = fetch s
    mod <- effect instr (memory s)
    return $ apply mod (s { curr_ip = next_ip })

-- a list with something else on the end
data ListPlus a b = Cons a (ListPlus a b) | End b
    deriving Show

listPlus :: (b -> Maybe (Maybe a, b)) -> b -> ListPlus a b
listPlus step s = case step s of
    Nothing -> End s
    Just (Nothing, s') -> listPlus step s'
    Just (Just v, s') -> Cons v (listPlus step s')

initLP :: ListPlus a b -> [a]
initLP (End _) = []
initLP (Cons x rest) = x : initLP rest

lastLP :: ListPlus a b -> b
lastLP (End y) = y
lastLP (Cons _ rest) = lastLP rest

-- Run the machine until it halts.
run :: Memory -> Memory
run = snd . runIO []

-- Run the machine with input, yielding output and final memory.
-- The pair and the output list are produced lazily.
runIO :: [Value] -> Memory -> ([Value], Memory)
runIO vs mem = (initLP r, memory (lastLP r))
  where
    r = listPlus advance (initState vs mem)

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
    return $ ((curr_ip s, instr, mod),
        snd (apply mod ( s { curr_ip = next_ip })))

-- Run the program with several inputs and return those for which it
-- produces different outputs than the function.
testProgram :: Memory -> (Value -> Value) -> [Value] -> [Value]
testProgram mem f xs = [x | x <- xs, fst (runIO [x] mem) /= [f x]]
