module Main where

import Utilities
import Intcode
import Data.List

-- Input processing

type Input = Memory

parse :: String -> Input
parse = readMemory

-- Part One

-- amplifier taking a single input and producing a single output
amplifier :: Memory -> Value -> Value -> Value
amplifier mem input phase = head (function mem [phase, input])

-- initial input value
initValue :: Value
initValue = 0

-- connect amplifiers with different phases in series
signal :: Memory -> [Value] -> Value
signal mem vs = foldl (amplifier mem) initValue vs

-- highest output for all possible permutations of the phases
solve1 :: Input -> Value
solve1 mem = maximum [signal mem vs | vs <- permutations [0..4]]

tests1 :: [(String, Value)]
tests1 = [
    ("3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0", 43210),
    ("3,23,3,24,1002,24,10,24,1002,23,-1,23,\
     \101,5,23,23,1,24,23,23,4,23,99,0,0", 54321),
    ("3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,\
     \1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0", 65210)]

-- Part Two

-- amplifier taking multiple inputs and producing multiple outputs
multi_amplifier :: Memory -> [Value] -> Value -> [Value]
multi_amplifier mem vs phase = function mem (phase:vs)

-- last output of a feedback loop of a series of amplifiers with different
-- phases, given the initial input (zero)
feedback :: Memory -> [Value] -> Value
feedback mem vs = last outputs
  where
    outputs = foldl (multi_amplifier mem) (initValue:outputs) vs

-- highest output for all possible permutations of the feedback phases
solve2 :: Input -> Value
solve2 mem = maximum [feedback mem vs | vs <- permutations [5..9]]

tests2 :: [(String, Value)]
tests2 = [
    ("3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,\
     \27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5", 139629729),
    ("3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,\
     \-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,\
     \53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10", 18216)]

main :: IO ()
main = do
    s <- readFile "input/07.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
