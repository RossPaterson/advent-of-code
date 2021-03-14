module Main where

import AssemblyCode
import Matching
import Parser
import Utilities
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- Input processing

type Input = ([Sample], [RawInstruction])

type Sample = (State, RawInstruction, State)
type RawInstruction = GenInstruction Int

parse :: String -> Input
parse s = (map parseSample samples, map (runParser (instruction int)) code)
  where
    (samples, code) = splitSamples (lines s)

parseSample :: String -> Sample
parseSample = runParser sample
  where
    sample = (,,) <$
        string "Before: " <*> state <* char '\n' <*>
        instruction int <* char '\n' <*
        string "After:  " <*> state <* string "\n\n"
    state = mkState <$ string "[" <*> sepBy1 nat (string ", ") <* char ']'
    mkState vs = Map.fromList (zip [0..] vs)

splitSamples :: [String] -> ([String], [String])
splitSamples ls = (map unlines (init (takes 4 sample_lines)), code_lines)
  where
    n = length ls
    code_len = length (takeWhile (not . null) (reverse ls))
    sample_lines = take (n - code_len) ls
    code_lines = drop (n - code_len) ls

-- Part One

solve1 :: Input -> Int
solve1 (samples, _code) = length [s | s <- samples, length (possibles s) >= 3]

-- Possible opcodes for the instruction in a given sample
possibles :: Sample -> [OpCode]
possibles (before, instr, after) = [opcode |
    opcode <- allValues,
    execute before (opcode <$ instr) == after]

tests1 :: [(String, [OpCode])]
tests1 = [(testInput, [ADDI,MULR,SETI])]

testInput :: String
testInput =
    "Before: [3, 2, 1, 1]\n\
    \9 2 1 2\n\
    \After:  [3, 2, 2, 1]\n\
    \\n"

-- Part Two

-- identify the opcodes, and run the translated code
solve2 :: Input -> Int
solve2 (samples, code) =
    foldl execute (initState 4) (map (fmap (table!)) code) ! 0
  where
    table = resolve samples

-- The opcodes for each value consistent with the samples
resolve :: [Sample] -> Map Int OpCode
resolve = uniquePerfectMatching . analyse

-- The set of possible opcodes for each value consistent with the samples
analyse :: [Sample] -> Map Int (Set OpCode)
analyse samples =
    Map.unionsWith Set.intersection
        [Map.singleton opcode (Set.fromList (possibles s)) |
            s@(_, Instruction opcode _ _ _, _) <- samples]

main :: IO ()
main = do
    s <- readFile "input/16.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (possibles . parseSample) tests1))
    print (solve1 input)
    print (solve2 input)
