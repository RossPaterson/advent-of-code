module Main where

import Utilities
import Data.List

-- Input processing

type Input = String

parse :: String -> Input
parse s = head (lines s)

-- Part One

allDifferent :: Eq a => [a] -> Bool
allDifferent [] = True
allDifferent (x:xs) = not (elem x xs) && allDifferent xs

marker :: Eq a => Int -> [a] -> Bool
marker n xs = length xs >= n && allDifferent (take n (reverse xs))

firstMarker :: Eq a => Int -> [a] -> Int
firstMarker n = length . takeWhile (not . marker n) . inits

solve1 :: Input -> Int
solve1 = firstMarker 4

tests1 :: [(String, Int)]
tests1 = [
    ("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 7),
    ("bvwbjplbgvbhsrlpgdmjqwftvncz", 5),
    ("nppdvjthqldpwncqszvftbrmjlhg", 6),
    ("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 10),
    ("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 11)]

-- Part Two

solve2 :: Input -> Int
solve2 = firstMarker 14

tests2 :: [(String, Int)]
tests2 = [
    ("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 19),
    ("bvwbjplbgvbhsrlpgdmjqwftvncz", 23),
    ("nppdvjthqldpwncqszvftbrmjlhg", 23),
    ("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 29),
    ("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 26)]

main :: IO ()
main = do
    s <- readFile "input/06.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
