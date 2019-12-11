module Main where

import Intcode
import Utilities
import Data.Map (Map, (!))
import qualified Data.Map as Map

-- Input processing

type Input = Memory

parse :: String -> Input
parse = readMemory

-- Part One

initialize :: Value -> Value -> Memory -> Memory
initialize n v = Map.insert 1 n . Map.insert 2 v

output :: Memory -> Value
output mem = mem!0

solve1 :: Input -> Value
solve1 = output . run . initialize 12 2

tests1 :: [(String, [Value])]
tests1 = [
    ("1,9,10,3,2,3,11,0,99,30,40,50", [3500,9,10,70,2,3,11,0,99,30,40,50]),
    ("1,0,0,0,99", [2,0,0,0,99]),
    ("2,3,0,3,99", [2,3,0,6,99]),
    ("2,4,4,5,99,0", [2,4,4,5,99,9801]),
    ("1,1,1,4,99,5,6,0,99", [30,1,1,4,2,5,6,0,99])]

-- Part Two

target2 :: Value
target2 = 19690720

solve2 :: Input -> Value
solve2 mem = head [100 * noun + verb | noun <- [0..99], verb <- [0..99],
    output (run (initialize noun verb mem)) == target2]

main :: IO ()
main = do
    s <- readFile "input/02.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (Map.elems . run . parse) tests1))
    print (solve1 input)
    print (solve2 input)
