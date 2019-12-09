-- toy computer for Days 2, 5, 7, 9, ...
module Intcode where

import Utilities
import Data.List
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Maybe

type Memory = Map Address Value
type Address = Integer
type Value = Integer

-- comma-separated list of values for locations 0..
readMemory :: String -> Memory
readMemory s = Map.fromAscList $ zip [0..] $
    map read (words [if c == ',' then ' ' else c | c <- s])

data State = State {
    memory :: Memory,
    curr_ip :: Address, -- instruction pointer
    rel_base :: Address, -- base address for Relative addressing
    inputs :: [Value] -- unread input values
    }
    deriving (Eq, Ord, Show)

initState :: [Value] -> Memory -> State
initState vs mem = State {
    memory = mem,
    curr_ip = 0,
    rel_base = 0,
    inputs = vs
    }

data Action
    = Set Address Value
    | ReadTo Address
    | Write Value
    | SetIP Value
    | SetRelBase Value
    | Nop
    deriving (Eq, Ord, Show)

-- an action may produce an output value, and can update the state
apply :: Action -> State -> (Maybe Value, State)
apply (Set addr v) s =
    (Nothing, s { memory = Map.insert addr v (memory s) })
apply (Write v) s = (Just v, s)
apply (ReadTo addr) s = case (inputs s) of
    v:vs -> (Nothing, s { memory = Map.insert addr v (memory s), inputs = vs })
    [] -> error "no input"
apply (SetIP addr) s = (Nothing, s { curr_ip = addr })
apply (SetRelBase v) s = (Nothing, s { rel_base = v })
apply Nop s = (Nothing, s)

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
    leadvalue = mem_lookup ip
    parameter n = mode n (mem_lookup (ip+n))
    mode n v = case leadvalue `div` (10^(n+1)) `mod` 10 of
        0 -> Position v
        1 -> Immediate v
        2 -> Relative v
        d -> exec_error $ "bad parameter mode " ++ show d
    mem_lookup addr
      | addr < 0 = exec_error $ "negative address " ++ show addr
      | otherwise = fromMaybe 0 (Map.lookup addr mem)
    exec_error msg = error (show ip ++ ": " ++ msg)

effect :: Instruction -> State -> Maybe Action
effect instr s = case instr of
    Add x y r -> Just (Set (target r) (get x + get y))
    Mul x y r -> Just (Set (target r) (get x * get y))
    Input r -> Just (ReadTo (target r))
    Output x -> Just (Write (get x))
    JumpIfTrue x y -> Just (if get x /= 0 then SetIP (get y) else Nop)
    JumpIfFalse x y -> Just (if get x == 0 then SetIP (get y) else Nop)
    LessThan x y r -> Just (Set (target r) (fromBool (get x < get y)))
    Equals x y r -> Just (Set (target r) (fromBool (get x == get y)))
    AdjustRelBase x -> Just (SetRelBase (base + get x))
    Halt -> Nothing
  where
    mem = memory s
    base = rel_base s
    mem_lookup addr
      | addr < 0 = error $ "negative address " ++ show addr
      | otherwise = fromMaybe 0 (Map.lookup addr mem)
    get (Position addr) = mem_lookup addr
    get (Immediate n) = n
    get (Relative n) = mem_lookup (base + n)
    target (Position addr) = addr
    target (Immediate n) = error $ "setting immediate " ++ show n
    target (Relative n) = base + n

fromBool :: Bool -> Value
fromBool = toInteger . fromEnum

-- fetch-execute cycle
advance :: State -> Maybe (Maybe Value, State)
advance s = do
    let (instr, next_ip) = fetch s
    mod <- effect instr s
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
    (f, b) = splitAt (fromInteger (curr_ip s)) (Map.elems (memory s))

trace :: Memory -> [(Address, Instruction, Action)]
trace = traceIO []

traceIO :: [Value] -> Memory -> [(Address, Instruction, Action)]
traceIO vs mem = unfoldr advanceTrace (initState vs mem)

advanceTrace :: State -> Maybe ((Address, Instruction, Action), State)
advanceTrace s = do
    let (instr, next_ip) = fetch s
    mod <- effect instr s
    return $ ((curr_ip s, instr, mod),
        snd (apply mod ( s { curr_ip = next_ip })))

-- semi-readable summary of execution history
-- (flaw: doesn't show the halt)
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
        (("\t" ++ showInstruction instr):showSummary summ)

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
