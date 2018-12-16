module Main where

import Parser
import Utilities
import Control.Applicative
import Data.Bits
import Data.Functor
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- Input processing

type Input = ([Sample], [Instruction Int])

type Sample = (State, Instruction Int, State)

type State = Map Register Int

data Instruction a = Instruction a Register Register Register
  deriving Show

instance Functor Instruction where
    fmap f (Instruction i a b c) = Instruction (f i) a b c

data OpCode =
    ADDR | ADDI | MULR | MULI | BANR | BANI | BORR | BORI |
    SETR | SETI | GTIR | GTRI | GTRR | EQIR | EQRI | EQRR
  deriving (Show, Eq, Ord, Bounded, Enum)
data Register = R0 | R1 | R2 | R3
  deriving (Show, Eq, Ord, Bounded, Enum)

parse :: String -> Input
parse s = (map parseSample samples, map (runParser instruction) code)
  where
    (samples, code) = splitSamples (lines s)
    instruction = Instruction <$>
        nat <* char ' ' <*> reg <* char ' ' <*> reg <* char ' ' <*> reg
    reg = toEnum <$> nat

parseSample :: String -> Sample
parseSample = runParser sample
  where
    sample = (,,) <$
        string "Before: " <*> state <* char '\n' <*>
        instruction <* char '\n' <*
        string "After:  " <*> state <* string "\n\n"
    state = mkState <$ string "[" <*> nat <* string ", " <*> nat <*
        string ", " <*> nat <* string ", " <*> nat <* char ']'
    instruction = Instruction <$>
        nat <* char ' ' <*> reg <* char ' ' <*> reg <* char ' ' <*> reg
    reg = toEnum <$> nat
    mkState v0 v1 v2 v3 = Map.fromList [(R0, v0), (R1, v1), (R2, v2), (R3, v3)]

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

-- execute a single instruction
execute :: State -> Instruction OpCode -> State
execute s (Instruction ADDR a b c) = Map.insert c (s!a + s!b) s
execute s (Instruction ADDI a b c) = Map.insert c (s!a + fromEnum b) s
execute s (Instruction MULR a b c) = Map.insert c (s!a * s!b) s
execute s (Instruction MULI a b c) = Map.insert c (s!a * fromEnum b) s
execute s (Instruction BANR a b c) = Map.insert c (s!a .&. s!b) s
execute s (Instruction BANI a b c) = Map.insert c (s!a .&. fromEnum b) s
execute s (Instruction BORR a b c) = Map.insert c (s!a .|. s!b) s
execute s (Instruction BORI a b c) = Map.insert c (s!a .|. fromEnum b) s
execute s (Instruction SETR a b c) = Map.insert c (s!a) s
execute s (Instruction SETI a b c) = Map.insert c (fromEnum a) s
execute s (Instruction GTIR a b c) =
    Map.insert c (fromEnum (fromEnum a > s!b)) s
execute s (Instruction GTRI a b c) =
    Map.insert c (fromEnum (s!a > fromEnum b)) s
execute s (Instruction GTRR a b c) =
    Map.insert c (fromEnum (s!a > s!b)) s
execute s (Instruction EQIR a b c) =
    Map.insert c (fromEnum (fromEnum a ==
    s!b)) s
execute s (Instruction EQRI a b c) =
    Map.insert c (fromEnum (s!a ==
    fromEnum b)) s
execute s (Instruction EQRR a b c) =
    Map.insert c (fromEnum (s!a == s!b)) s

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
    foldl execute initState (map (fmap (table!)) code) ! R0
  where
    table = resolve samples

-- Initially each register holds 0
initState :: State
initState = Map.fromList [(r, 0) | r <- allValues]

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
-- The dual strategy could also be used, but is not needed for this input.
reduce ::
   (Map Int OpCode, Map Int (Set OpCode)) ->
   (Map Int OpCode, Map Int (Set OpCode))
reduce (done, ambiguous) = (done', ambiguous')
  where
    unique =
        [(i, head (Set.toList s)) |
            (i, s) <- Map.toList ambiguous, Set.size s == 1]
    done' = Map.union done (Map.fromList unique)
    ambiguous' =
        compose [fmap (Set.delete opcode) | (i, opcode) <- unique] $
        foldr Map.delete ambiguous [i | (i, opcode) <- unique]

main :: IO ()
main = do
    s <- readFile "input16.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (possibles . parseSample) tests1))
    print (solve1 input)
    print (solve2 input)
