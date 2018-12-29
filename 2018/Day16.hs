module Main where

import AssemblyCode
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
solve1 (samples, code) = length [s | s <- samples, length (possibles s) >= 3]

-- Possible opcodes for the instruction in a given sample
possibles :: Sample -> [OpCode]
possibles (before, instr, after) = [opcode |
    opcode <- allValues,
    execute before (opcode <$ instr) == after]

tests1 :: [(String, [OpCode])]
tests1 = [(testInput, [ADDI,MULR,SETI])]

testInput = "\
\Before: [3, 2, 1, 1]\n\
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
resolve samples =
    fst $ until (Map.null . snd) reduce (Map.empty, analyse samples)

-- The set of possible opcodes for each value consistent with the samples
analyse :: [Sample] -> Map Int (Set OpCode)
analyse samples =
    Map.unionsWith Set.intersection
        [Map.singleton opcode (Set.fromList (possibles s)) |
         s@(before, Instruction opcode a b c, after) <- samples]

-- At each stage we have values for which we know the opcode and values
-- for each of which we have a set of opcodes.
-- Any value with a unique opcode can be assigned, and both value and
-- opcode removed from the remaining possibilities.
-- The same can be done for any opcode with a unique value.
-- Both strategies are implemented here, but the first suffices for this input.
reduce ::
   (Map Int OpCode, Map Int (Set OpCode)) ->
   (Map Int OpCode, Map Int (Set OpCode))
reduce (done, ambiguous) = (done', ambiguous')
  where
    unique =
        fast_nub (singletons ambiguous ++
            map swap (singletons (invert ambiguous)))
    done' = Map.union done (Map.fromList unique)
    ambiguous' =
        compose [fmap (Set.delete opcode) | (i, opcode) <- unique] $
        foldr Map.delete ambiguous [i | (i, opcode) <- unique]

-- mappings to a unique value
singletons :: (Ord a, Ord b) => Map a (Set b) -> [(a, b)]
singletons m =
    [(i, head (Set.toList s)) | (i, s) <- Map.toList m, Set.size s == 1]

-- invert a relation
invert :: (Ord a, Ord b) => Map a (Set b) -> Map b (Set a)
invert m =
    Map.unionsWith Set.union
        [Map.singleton y (Set.singleton x) |
            (x, s) <- Map.toList m, y <- Set.toList s]

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

main :: IO ()
main = do
    s <- readFile "input/16.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (possibles . parseSample) tests1))
    print (solve1 input)
    print (solve2 input)
