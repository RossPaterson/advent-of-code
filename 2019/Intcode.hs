-- toy computer for Days 2, 5, 7, 9, 11, ...
module Intcode(
    Memory, readMemory, setMemory, getMemory, contents,
    Address, Value, toValue, fromValue,
    -- pure function
    run, streamFunction,
    -- debugging
    trace, showSummaryTrace, showState,
    -- interactive interface
    Automaton(..), automaton, runPartial,
    ListPlus(..), initLP, lastLP
    ) where

import Data.Map (Map)
import qualified Data.Map as Map

newtype Memory = Memory (Map Address Value)
    deriving (Eq, Ord, Show)
type Address = Integer
type Value = Integer

toValue :: Enum a => a -> Value
toValue = toInteger . fromEnum

fromValue :: Enum a => Value -> a
fromValue = toEnum . fromInteger

-- comma-separated list of values for locations 0..
readMemory :: String -> Memory
readMemory s = Memory $ Map.fromAscList $ zip [0..] $
    map read (words [if c == ',' then ' ' else c | c <- s])

setMemory :: Address -> Value -> Memory -> Memory
setMemory addr v (Memory m)
  | addr < 0 = error ("negative address " ++ show addr)
  | otherwise = Memory (Map.insert addr v m)

getMemory :: Address -> Memory -> Value
getMemory addr (Memory m)
  | addr < 0 = error ("negative address " ++ show addr)
  | otherwise = Map.findWithDefault 0 addr m

contents :: Memory -> [Value]
contents (Memory m) = [Map.findWithDefault 0 addr m | addr <- [0..top]]
  where
    top = fst (Map.findMax m)

data State = State {
    memory :: Memory,
    curr_ip :: Address, -- instruction pointer
    rel_base :: Address -- base address for Relative addressing
    }
    deriving (Eq, Ord, Show)

initState :: Memory -> State
initState mem = State {
    memory = mem,
    curr_ip = 0,
    rel_base = 0
    }

data Action
    = Set Address Value
    | ReadTo Address
    | Write Value
    | SetIP Value
    | SetRelBase Value
    | Nop
    | Stop
    deriving (Eq, Ord, Show)

-- an action may produce an output value, and can update the state
apply :: Action -> State -> State
apply (Set addr v) s = s { memory = setMemory addr v (memory s) }
apply (Write _) _ = error "Write passed to apply"
apply (ReadTo _) _ = error "ReadTo passed to apply"
apply (SetIP addr) s = s { curr_ip = addr }
apply (SetRelBase v) s = s { rel_base = v }
apply Nop s = s
apply Stop _ = error "Stop passed to apply"

data Instruction
    = Add Parameter Parameter Parameter -- Day 2
    | Mul Parameter Parameter Parameter -- Day 2
    | Input Parameter -- Day 5 part 1
    | Output Parameter -- Day 5 part 1
    | JumpIfTrue Parameter Parameter -- Day 5 part 2
    | JumpIfFalse Parameter Parameter -- Day 5 part 2
    | LessThan Parameter Parameter Parameter -- Day 5 part 2
    | Equals Parameter Parameter Parameter -- Day 5 part 2
    | AdjustRelBase Parameter -- Day 9
    | Halt -- Day 2
    deriving (Eq, Ord, Show)

-- instruction parameter
data Parameter = Position Address | Immediate Value | Relative Value
    deriving (Eq, Ord, Show)

-- returns decoded instruction and location of the following instruction
fetch :: State -> (Instruction, Address)
fetch s = case leadvalue `mod` 100 of
    1 -> (Add (parameter 1) (parameter 2) (parameter 3), ip+4)
    2 -> (Mul (parameter 1) (parameter 2) (parameter 3), ip+4)
    3 -> (Input (parameter 1), ip+2)
    4 -> (Output (parameter 1), ip+2)
    5 -> (JumpIfTrue (parameter 1) (parameter 2), ip+3)
    6 -> (JumpIfFalse (parameter 1) (parameter 2), ip+3)
    7 -> (LessThan (parameter 1) (parameter 2) (parameter 3), ip+4)
    8 -> (Equals (parameter 1) (parameter 2) (parameter 3), ip+4)
    9 -> (AdjustRelBase (parameter 1), ip+2)
    99 -> (Halt, ip+1)
    n -> exec_error $ "Bad opcode " ++ show n
  where
    mem = memory s
    ip = curr_ip s
    leadvalue = getMemory ip mem
    parameter n = mode n (getMemory (ip+n) mem)
    mode n v = case leadvalue `div` (10^(n+1)) `mod` 10 of
        0 -> Position v
        1 -> Immediate v
        2 -> Relative v
        d -> exec_error $ "bad parameter mode " ++ show d
    exec_error msg = error (show ip ++ ": " ++ msg)

effect :: Instruction -> State -> Action
effect instr s = case instr of
    Add x y r -> Set (target r) (get x + get y)
    Mul x y r -> Set (target r) (get x * get y)
    Input r -> ReadTo (target r)
    Output x -> Write (get x)
    JumpIfTrue x y -> if get x /= 0 then SetIP (get y) else Nop
    JumpIfFalse x y -> if get x == 0 then SetIP (get y) else Nop
    LessThan x y r -> Set (target r) (fromBool (get x < get y))
    Equals x y r -> Set (target r) (fromBool (get x == get y))
    AdjustRelBase x -> SetRelBase (base + get x)
    Halt -> Stop
  where
    mem = memory s
    base = rel_base s
    get (Position addr) = getMemory addr mem
    get (Immediate n) = n
    get (Relative n) = getMemory (base + n) mem
    target (Position addr) = addr
    target (Immediate n) = error $ "setting immediate " ++ show n
    target (Relative n) = base + n

fromBool :: Bool -> Value
fromBool = toInteger . fromEnum

-- a list with something else on the end
data ListPlus a b = Cons a (ListPlus a b) | End b
    deriving Show

unfoldLP :: (b -> Maybe (a, b)) -> b -> ListPlus a b
unfoldLP step s = case step s of
    Nothing -> End s
    Just (v, s') -> Cons v (unfoldLP step s')

initLP :: ListPlus a b -> [a]
initLP (End _) = []
initLP (Cons x rest) = x : initLP rest

lastLP :: ListPlus a b -> b
lastLP (End y) = y
lastLP (Cons _ rest) = lastLP rest

splitLP :: ListPlus a b -> ([a], b)
splitLP lp = (initLP lp, lastLP lp)

automaton :: Memory -> Automaton
automaton = runState . initState

-- fetch-execute cycle
runState :: State -> Automaton
runState s = case effect instr s of
    Stop -> Finish s
    Write v -> WriteValue v (runState s')
    ReadTo addr -> ReadValue $ \ v ->
        runState (s' { memory = setMemory addr v (memory s') })
    act -> runState (apply act s')
  where
    (instr, next_ip) = fetch s
    s' = s { curr_ip = next_ip }

-- Run the machine until it halts, returning the final memory.
run :: Memory -> Memory
run mem = memory (lastLP (runAutomaton (automaton mem) []))

-- Lazy function from input to output defined by the machine.
streamFunction :: Memory -> [Value] -> [Value]
streamFunction mem = initLP . runAutomaton (automaton mem)

-- debugging

showState :: State -> String
showState s = unwords (map show f) ++ "." ++ unwords (map show b)
  where
    (f, b) = splitAt (fromInteger (curr_ip s)) (Map.elems m)
    Memory m = memory s

trace :: Memory -> [Value] -> [(Address, Instruction, Action)]
trace mem vs = initLP result ++ [(curr_ip s, Halt, Stop)]
  where
    result = unfoldLP advanceTrace (initState mem, vs)
    (s, _) = lastLP result

advanceTrace ::
    (State, [Value]) -> Maybe ((Address, Instruction, Action), (State, [Value]))
advanceTrace (s, vs) = case act of
    Stop -> Nothing
    Write _ -> Just (entry, (s', vs))
    ReadTo addr -> case vs of
        [] -> error "unexpected end of input"
        v:vs' ->
            Just (entry, (s' { memory = setMemory addr v (memory s') }, vs'))
    _ -> Just (entry, (apply act s', vs))
  where
    (instr, next_ip) = fetch s
    act = effect instr s
    entry = (curr_ip s, instr, act)
    s' = s { curr_ip = next_ip }

-- semi-readable summary of execution history
showSummaryTrace :: [(Address, Instruction, Action)] -> String
showSummaryTrace = unlines . concatMap showLine . summaryTrace

-- summary trace of an execution
summaryTrace :: [(Address, Instruction, Action)] ->
    [((Address, Instruction), Summary Action)]
summaryTrace = summarize . map (\(x, y, z) -> ((x, y), z))

-- record a count of occurrences, but don't keep more than two values
data Summary a
    = Single !Int a
    | Multiple !Int a !Int a !Int
    deriving Show

addSummary :: Eq a => a -> Maybe (Summary a) -> Summary a
addSummary x Nothing = Single 1 x
addSummary x (Just (Single na a))
  | x == a = Single (na+1) a
  | otherwise = Multiple na a 1 x 0
addSummary x (Just (Multiple na a nb b n))
  | x == a = Multiple (na+1) a nb b n
  | x == b = Multiple na a (nb+1) b n
  | otherwise = Multiple na a nb b (n+1)

summarize :: (Ord k, Eq v) => [(k, v)] -> [(k, Summary v)]
summarize = Map.assocs . foldl add Map.empty
  where
    add m (k, v) = Map.alter (Just . addSummary v) k m

-- semi-readable presentation of a trace line
showLine :: ((Address, Instruction), Summary Action) -> [String]
showLine ((addr, instr), summ) =
    map ((show addr ++ ":\t") ++)
        (showInstruction instr:showSummary summ)

showSummary :: Summary Action -> [String]
showSummary (Single na a) = [show na ++ "\t" ++ showAction a]
showSummary (Multiple na a nb b n) =
    [show na ++ "\t" ++ showAction a, show nb ++ "\t" ++ showAction b] ++
        if n > 0 then [show n ++ "\t???"] else []

showAction :: Action -> String
showAction (Set addr v) = showAddress addr ++ " = " ++ show v
showAction (ReadTo addr) = "read " ++ showAddress addr
showAction (Write v) = "write " ++ show v
showAction (SetIP v) = "goto " ++ show v
showAction (SetRelBase v) = "fp = " ++ show v
showAction Nop = "nop"
showAction Stop = "stop"

showInstruction :: Instruction -> String
showInstruction (Add a b c) =
    showParameter c ++ " = " ++ showParameter a ++ " + " ++ showParameter b
showInstruction (Mul a b c) =
    showParameter c ++ " = " ++ showParameter a ++ " * " ++ showParameter b
showInstruction (Input a) = "input " ++ showParameter a
showInstruction (Output a) = "output " ++ showParameter a
showInstruction (JumpIfTrue a b) =
    "if (" ++ showParameter a ++ ") goto " ++ showParameter b
showInstruction (JumpIfFalse a b) =
    "if (!" ++ showParameter a ++ ") goto " ++ showParameter b
showInstruction (LessThan a b c) =
    showParameter c ++ " = " ++ showParameter a ++ " < " ++ showParameter b
showInstruction (Equals a b c) =
    showParameter c ++ " = " ++ showParameter a ++ " == " ++ showParameter b
showInstruction (AdjustRelBase a) = "fp += " ++ showParameter a
showInstruction Halt = "halt"

showParameter :: Parameter -> String
showParameter (Position addr) = showAddress addr
showParameter (Immediate v) = show v
showParameter (Relative v)
  | v < 0 = "*(fp" ++ show v ++ ")"
  | v == 0 = "*fp"
  | otherwise = "*(fp+" ++ show v ++ ")"

showAddress :: Address -> String
showAddress addr = "m" ++ show addr

-- resumption-based interface

data Automaton
    = ReadValue (Value -> Automaton)
    | WriteValue Value Automaton
    | Finish State

runAutomaton :: Automaton -> [Value] -> ListPlus Value State
runAutomaton (ReadValue k) (v:vs) = runAutomaton (k v) vs
runAutomaton (ReadValue _) [] = error "no input left"
runAutomaton (WriteValue r a) vs = Cons r (runAutomaton a vs)
runAutomaton (Finish s) _ = End s

-- run until input exhausted
runPartial :: Automaton -> [Value] -> ([Value], Automaton)
runPartial a input = splitLP (part_run a input)
  where
    part_run (ReadValue k) (v:vs) = part_run (k v) vs
    part_run (WriteValue r a') vs = Cons r (part_run a' vs)
    part_run a' _ = End a'
