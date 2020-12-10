module Main where

import Utilities
import Data.List
import Data.Maybe

-- Input processing

type Input = [Int]

parse :: String -> Input
parse = map read . lines

-- Part One

-- differences between neighbouring values, with zero added at the start
-- and a gap of 3 added at the end
diffs :: [Int] -> [Int]
diffs vs = zipWith (-) sorted (0:sorted) ++ [3]
  where
    sorted = sort vs

solve1 :: Input -> Int
solve1 vs = freq 1 * freq 3
  where
    freqs = frequency (diffs vs)
    freq n = fromMaybe 0 (lookup n freqs) 

testInput1 :: String
testInput1 = "\
    \16\n\
    \10\n\
    \15\n\
    \5\n\
    \1\n\
    \11\n\
    \7\n\
    \19\n\
    \6\n\
    \12\n\
    \4\n"

testInput2 :: String
testInput2 = "\
    \28\n\
    \33\n\
    \18\n\
    \42\n\
    \31\n\
    \14\n\
    \46\n\
    \20\n\
    \48\n\
    \47\n\
    \24\n\
    \23\n\
    \49\n\
    \45\n\
    \19\n\
    \38\n\
    \39\n\
    \11\n\
    \1\n\
    \32\n\
    \25\n\
    \35\n\
    \8\n\
    \17\n\
    \7\n\
    \9\n\
    \4\n\
    \2\n\
    \34\n\
    \10\n\
    \3\n"

tests1 :: [(String, Int)]
tests1 = [(testInput1, 35), (testInput2, 220)]

-- Part Two

{-
Although the problem description doesn't say so, neither the test
inputs nor the actual input contain a difference of 2.  Since two
values separated by 3 must both be used, we need only consider
the number of ways of dropping values from some number of adjacent
values differing by one.

* The number of possibilities for each sublist of adjacent values depends
  only on the number of values.  In particular, if there are n adjacent
  differences of 1, i.e. n+1 adjacent values in the input, the number
  of selections with no gap of more than 3 is Tribonacci number a(n+2)
  (https://oeis.org/A000073), which can be proved by induction.  (In the
  actual input, the largest n is 4.)

* Since the sublists of adjacent values are independent, multiplying
  the number of possibilities for each group together gives the total
  number of combinations.
-}

-- lengths of sublists of adjacent ones in the input list
adjacent_ones :: [Int] -> [Int]
adjacent_ones xs = [length g | g <- group xs, head g == 1]

-- Tribonacci numbers (https://oeis.org/A000073) left shifted by 2
tribonacci :: Int -> Int
tribonacci n = t
  where
    (_, _, t) = times n (\ (a, b, c) -> (b, c, a+b+c)) (0, 0, 1)

solve2 :: Input -> Int
solve2 = product . map tribonacci . adjacent_ones . diffs

-- dynamic programming version: also works with gaps of 2
-- Pair each value with the number of valid sublists of the list from
-- that point on that include the value.
solve2DP :: Input -> Int
solve2DP vs = fst $ head $ foldr add_one [(1, maximum vs+3)] $ 0:sort vs
  where
    add_one x nvs = (nx, x):nvs
      where
        nx = sum $ map fst $ takeWhile (\ (_, x') -> x' <= x+3) nvs

tests2 :: [(String, Int)]
tests2 = [(testInput1, 8), (testInput2, 19208)]

main :: IO ()
main = do
    s <- readFile "input/10.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    putStr (unlines (failures "solve2DP" (solve2DP . parse) tests2))
    print (solve2 input)
