module Main where

import Utilities
import Data.List

-- Input processing

type Input = [String]

parse :: String -> Input
parse = lines

-- Part One

solve1 :: Input -> Int
solve1 = checksum

-- number of lists containing exactly two of any value multiplied by
-- number of lists containing exactly three of any value
checksum :: Ord a => [[a]] -> Int
checksum xss = product [length (filter (k `elem`) nss) | k <- [2, 3]]
  where
    nss = map (map snd . frequency) xss

tests1 :: [(Input, Int)]
tests1 = [(["abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab"], 12)]

-- Part Two

solve2 :: Input -> String
solve2 = head . nearRepetitions

-- shared parts of lists that are repeated except for the value at one position
-- The cost for n lists of length k is O(nk log n).
nearRepetitions :: Ord a => [[a]] -> [[a]]
nearRepetitions xss =
    [rest | ((_, rest), n) <- frequency (sort (concatMap removals xss)), n > 1]

-- All the ways of removing one element from a list.
-- Each pair (p, ys) is a position p and a list ys obtained by removing the
-- element of xs at position p.
removals :: [a] -> [(Int, [a])]
removals xs = [(length front, front++back) | (front, _:back) <- splits xs]

-- Another way of finding the shared part of a list that is repeated
-- except for the value at one position.
-- This implementation compares each list with every other list
-- The cost is O(n^2 k), but it is faster for the supplied input.
solve2_quadratic :: Ord a => [[a]] -> [a]
solve2_quadratic xss =
    head [[x1 | (x1, x2) <- zip xs1 xs2, x1 == x2] |
        xs1 <- xss, xs2 <- xss,
        length (filter id (zipWith (/=) xs1 xs2)) == 1]

tests2 :: [(Input, String)]
tests2 = [(["abcde", "fghij", "klmno", "pqrst", "fguij", "axcye", "wvxyz"], "fgij")]

main :: IO ()
main = do
    s <- readFile "input/02.txt"
    let input = parse s
    putStr (unlines (failures "solve1" solve1 tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" solve2 tests2))
    putStrLn (solve2 input)
