module Main where

import Assembly
import Utilities
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Input processing

type Input = Program

parse :: String -> Input
parse = parseProgram

-- Part One

-- states from the first time the pc repeats
from_first_rep :: Program -> [State]
from_first_rep p =
    map fst $ dropWhile (not . repeated) $ zip ss (initSets (map pc ss))
  where
    repeated (s, locs) = Set.member (pc s) locs -- been here before?
    ss = states p

solve1 :: Input -> Int
solve1 = accumulator . head . from_first_rep

testInput :: String
testInput = "\
    \nop +0\n\
    \acc +1\n\
    \jmp +4\n\
    \acc +3\n\
    \jmp -3\n\
    \acc -99\n\
    \acc +1\n\
    \jmp -4\n\
    \acc +6\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 5)]

-- Part Two

-- try to change an instruction
change :: Instruction -> Maybe Instruction
change (Jmp n) = Just (Nop n)
change (Nop n) = Just (Jmp n)
change _ = Nothing

-- ways of changing exactly one instruction in the program
patches :: Program -> [Program]
patches p =
    [Map.insert loc i' p |
        (loc, i) <- Map.assocs p, i' <- maybeToList (change i)]

-- does this program terminate?
terminates :: Program -> Bool
terminates = null . from_first_rep

-- apply a patch that makes the program terminate
repair :: Program -> Program
repair = head . filter terminates . patches

solve2 :: Input -> Int
solve2 = accumulator . runProgram . repair

tests2 :: [(String, Int)]
tests2 = [(testInput, 8)]

main :: IO ()
main = do
    s <- readFile "input/08.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
