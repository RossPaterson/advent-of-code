module Main where

import Parser
import Utilities
import Control.Applicative
import Data.Array
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq, (|>), ViewL(..))
import qualified Data.Sequence as Seq

type Reg = Char
data Source = Reg Reg | Value Int
    deriving Show
data Instruction
    = Snd Source
    | Set Reg Source
    | Add Reg Source
    | Mul Reg Source
    | Mod Reg Source
    | Rcv Reg
    | Jgz Source Source
    deriving Show

type Input = [Instruction]

parse :: String -> Input
parse = map (runParser instruction) . lines
  where
    instruction =
        Snd <$ string "snd " <*> source <|>
        Set <$ string "set " <*> reg <* string " " <*> source <|>
        Add <$ string "add " <*> reg <* string " " <*> source <|>
        Mul <$ string "mul " <*> reg <* string " " <*> source <|>
        Mod <$ string "mod " <*> reg <* string " " <*> source <|>
        Rcv <$ string "rcv " <*> reg <|>
        Jgz <$ string "jgz " <*> source <* string " " <*> source
    source = Reg <$> reg <|> Value <$> int
    reg = satisfy isLower

type Code = Array Int Instruction

code :: Input -> Code
code instrs = listArray (0, length instrs-1) instrs

-- State of a thread

data Thread = Thread {
    pc :: Int,
    register :: Map Char Int }

initThread :: Thread
initThread = Thread { pc = 0, register = Map.empty }

get :: Thread -> Source -> Int
get s (Reg r) = Map.findWithDefault 0 r (register s)
get _ (Value v) = v

set :: Thread -> Reg -> Int -> Thread
set s r v = s { register = Map.insert r v (register s) }

advance :: Thread -> Thread
advance s = s { pc = pc s + 1 }

execute :: Thread -> Instruction -> Thread
execute s (Snd x) = error "undefined snd instruction"
execute s (Set r x) = advance (set s r (get s x))
execute s (Add r x) = advance (set s r (get s (Reg r) + get s x))
execute s (Mul r x) = advance (set s r (get s (Reg r) * get s x))
execute s (Mod r x) = advance (set s r (get s (Reg r) `mod` get s x))
execute s (Rcv r) = error "undefined rcv instruction"
execute s (Jgz x y)
  | get s x > 0 = s { pc = pc s + get s y }
  | otherwise = advance s

-- Part 1: remember the last thing sent

type State1 = (Thread, Int)

initState1 :: State1
initState1 = (initThread, 0)

execute1 :: State1 -> Instruction -> State1
execute1 (s, _) (Snd x) = (advance s, get s x)
execute1 (s, v) (Rcv r)
  | get s (Reg r) /= 0 = (s { pc = -1 }, v)
  | otherwise = (advance s, v)
execute1 (s, v) i = (execute s i, v)

step1 :: Code -> State1 -> Maybe State1
step1 program s
  | inRange (bounds program) loc = Just (execute1 s (program!loc))
  | otherwise = Nothing
  where
    loc = pc (fst s)

run1 :: Code -> State1
run1 program = whileJust (step1 program) initState1

solve1 :: Input -> Int
solve1 = snd . run1 . code

testInput :: String
testInput = "\
    \set a 1\n\
    \add a 2\n\
    \mul a a\n\
    \mod a 5\n\
    \snd a\n\
    \set a 0\n\
    \rcv a\n\
    \jgz a -1\n\
    \set a 1\n\
    \jgz a -2\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 4)]

-- Part Two

-- Communication channel between threads

data Channel a = Channel { queue :: Seq a, sendCount :: Int }

initChannel :: Channel a
initChannel = Channel { queue = Seq.empty, sendCount = 0 }

emptyChannel :: Channel a -> Bool
emptyChannel = Seq.null . queue

send :: a -> Channel a -> Channel a
send v c = Channel { queue = queue c |> v, sendCount = sendCount c + 1 }

receive :: Channel a -> Maybe (a, Channel a)
receive c = case Seq.viewl (queue c) of
    EmptyL -> Nothing
    v :< rest -> Just (v, c { queue = rest })

-- Environment of a single thread

data TState = TState {
    state :: Thread,
    inq :: Channel Int,
    outq :: Channel Int }

execute2 :: TState -> Instruction -> TState
execute2 s (Snd x) =
    s { state = advance (state s), outq = send (get (state s) x) (outq s) }
execute2 s (Rcv r) = case receive (inq s) of
    Nothing -> s
    Just (v, rest) -> s { state = advance (set (state s) r v), inq = rest }
execute2 s instr = s { state = execute (state s) instr }

-- State of two communicating processes

data State2 = State2 {
    thread :: Array Int Thread,
    output :: Array Int (Channel Int),
    current :: Int }

initState2 :: State2
initState2 = State2 {
    thread = listArray (0,1) [set initThread 'p' 0, set initThread 'p' 1],
    output = listArray (0,1) [initChannel, initChannel],
    current = 0 }

running :: Code -> State2 -> Int -> Bool
running program s this
  | inRange (bounds program) loc = case program!loc of
        Rcv _ -> not (emptyChannel (output s!other))
        _ -> True
  | otherwise = False
  where
    other = 1 - this
    loc = pc (thread s!this)

anyRunning :: Code -> State2 -> Bool
anyRunning program s = any (running program s) [0, 1]

step2 :: Code -> State2 -> State2
step2 program s
  | inRange (bounds program) (pc (thread s!this)) = s {
        thread = array (0,1) [(this, state ts'), (other, thread s!other)],
        output = array (0,1) [(this, outq ts'), (other, inq ts')],
        current = other }
  | otherwise = s { current = other }
  where
    this = current s
    other = 1 - this
    ts = TState {
        state = thread s!this,
        inq = output s!other,
        outq = output s!this }
    ts' = execute2 ts (program!pc (thread s!this))

run2 :: Code -> State2
run2 program = until (not . anyRunning program) (step2 program) initState2

solve2 :: Input -> Int
solve2 xs = sendCount (output (run2 (code xs))!1)

testInput2 :: String
testInput2 = "\
    \snd 1\n\
    \snd 2\n\
    \snd p\n\
    \rcv a\n\
    \rcv b\n\
    \rcv c\n\
    \rcv d\n"

tests2 = [(testInput2, 3)]

main :: IO ()
main = do
    s <- readFile "input/18.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
