module Main where

import AssemblyCode
import Utilities
import Data.Map ((!))
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Input processing

type Input = Program

-- the first value compared with register 0
parse :: String -> Input
parse = parseProgram

-- Part One

-- The input program is equivalent to:
--
--     v = <initial value>;
--     do {
--         v = hash(v);
--     } while (v != r0);
--
-- so it suffices to look at values compared with register 0.

-- The above gives the answer, but is very slow.  The expensive part of
-- the hash function is an inner loop of the form
--
--     i = 0;
--     while ((i+1)*256 <= k)
--         i++;
--     k = i;
--
-- We can replace this with the equivalent
--
--     k = k/256;
--
speedup :: Program -> Program
speedup p = p { instructions = code' }
  where
    code = instructions p
    code' =
       Map.insert 17 (Instruction DIVI r c r) $
       Map.insert 18 (code!27) $
       code
    Instruction _ _ c _ = code!19
    Instruction _ _ _ r = code!26

solve1 :: Input -> Int
solve1 = head . values

-- values compared for equality with register 0
-- (assumes that there is a single such comparison in the program)
values :: Program -> [Int]
values p = map (! r) $ filter breakpoint $ allStates p numRegisters
  where
    breakpoint s = s!ip p == loc
    (loc, r) =
        head [(l, rv) |
            (l, Instruction EQRR rv 0 _) <- Map.toList (instructions p)]

numRegisters :: Int
numRegisters = 6

-- Part Two

-- the last value compared with register 0 before they start repeating
solve2 :: Input -> Int
solve2 = last . uniq . values

-- elements of a list before the first repetition
uniq :: Ord a => [a] -> [a]
uniq xs =
    map fst $ takeWhile (not . uncurry Set.member) $ zip xs (initSets xs)

main :: IO ()
main = do
    s <- readFile "input/21.txt"
    let input = speedup $ parse s
    print (solve1 input)
    print (solve2 input)
