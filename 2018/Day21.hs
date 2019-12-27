module Main where

import AssemblyCode
import Data.Map ((!))
import qualified Data.Map as Map
import Data.Set (Set)
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

solve1 :: Input -> Int
solve1 = head . values

-- values compared for equality with register 0
-- (assumes that there is a single such comparison in the program)
values :: Program -> [Int]
values p = map (!r) $ filter breakpoint $ allStates p numRegisters
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
    map fst $ takeWhile (not . uncurry Set.member) $ zip xs (init_sets xs)

-- same as map Set.fromList (inits xs)
init_sets :: Ord a => [a] -> [Set a]
init_sets xs = scanl (flip Set.insert) Set.empty xs

main :: IO ()
main = do
    s <- readFile "input/21.txt"
    let input = parse s
    print (solve1 input)
    print (solve2 input)
