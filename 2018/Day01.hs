module Main where

import Utilities
import Data.Set (Set)
import qualified Data.Set as Set

-- Input processing

type Input = [Int]

parse :: String -> Input
parse = map read . lines . filter (/= '+')

-- Part One

solve1 :: Input -> Int
solve1 = sum

tests1 :: [(Input, Int)]
tests1 = [
    ([1, -2, 3, 1], 3),
    ([1, 1, 1], 3),
    ([1, 1, -2], 0),
    ([-1, -2, -3], -6)]

-- Part Two

solve2 :: Input -> Int
solve2 = head . repeats . scanl (+) 0 . cycle

-- repeated elements of the list
repeats :: Ord a => [a] -> [a]
repeats xs = [x | (x, prev_xs) <- zip xs (init_sets xs), Set.member x prev_xs]

-- same as map Set.fromList (inits xs)
init_sets :: Ord a => [a] -> [Set a]
init_sets xs = scanl (flip Set.insert) Set.empty xs

tests2 :: [(Input, Int)]
tests2 = [
    ([1, -2, 3, 1], 2),
    ([1, -1], 0),
    ([3, 3, 4, -2, -4], 10),
    ([-6, 3, 8, 5, -6], 5),
    ([7, 7, -2, -7, -4], 14)]

main :: IO ()
main = do
    s <- readFile "input/01.txt"
    let input = parse s
    putStr (unlines (failures "solve1" solve1 tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" solve2 tests2))
    print (solve2 input)
