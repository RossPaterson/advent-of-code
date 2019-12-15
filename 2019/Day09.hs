module Main where

import Utilities
import Intcode

-- Input processing

type Input = Memory

parse :: String -> Input
parse = readMemory

-- Part One

solve1 :: Input -> Value
solve1 = head . flip streamFunction [1]

tests1 :: [(String, [Value])]
tests1 = [
    ("109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99",
     [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]),
    ("1102,34915192,34915192,7,4,7,99,0", [34915192^2]),
    ("104,1125899906842624,99", [1125899906842624])]

-- Part Two

solve2 :: Input -> Value
solve2 = head . flip streamFunction [2]

main :: IO ()
main = do
    s <- readFile "input/09.txt"
    let input = parse s
    putStr (unlines (failures "streamFunction" (flip streamFunction [] . parse) tests1))
    putStr $ showSummaryTrace $ trace input [2]
    print (solve1 input)
    print (solve2 input)
