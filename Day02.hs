module Main where

import Utilities
import Data.List

type Input = [String]

parse :: String -> Input
parse = lines

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

-- lists that are repeated except for the value at one position
nearRepetitions :: Ord a => [[a]] -> [[a]]
nearRepetitions xss =
    [rest | ((p, rest), n) <- frequency (sort (concatMap removals xss)), n > 1]

-- All the ways of removing one element from a list.
-- Each pair (p, ys) is a position p and a list ys obtained by removing the
-- element of xs at position p.
removals :: [a] -> [(Int, [a])]
removals xs =
    [(length front, front++back) | (front, x:back) <- zip (inits xs) (tails xs)]

tests2 :: [(Input, String)]
tests2 = [(["abcde", "fghij", "klmno", "pqrst", "fguij", "axcye", "wvxyz"], "fgij")]

main :: IO ()
main = do
    s <- readFile "input02.txt"
    let input = parse s
    putStr (unlines (failures "solve1" solve1 tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" solve2 tests2))
    putStrLn (solve2 input)
