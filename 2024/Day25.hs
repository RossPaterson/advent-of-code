module Main where

import Utilities
import Data.Either
import Data.List

-- Input processing

type Input = ([Lock], [Key])

-- amount of space in each column
type Lock = [Int]

-- amount filled in each column
type Key = [Int]

parse :: String -> Input
parse = partitionEithers . map (lockOrKey . lines) . paragraphs

-- Interpret a schematic as either a lock or a key
lockOrKey :: [String] -> Either Lock Key
lockOrKey ls
  | all (== '#') (head ls) = Left (heights '.' ls)
  | otherwise = Right (heights '#' ls)

-- number of occurrences of c in each column of the schematic
heights :: Char -> [String] -> [Int]
heights c = map (length . filter (== c)) . transpose

-- Part One

-- How many lock/key pairs fit together?
solve1 :: Input -> Int
solve1 (locks, keys) =
    length [(lock, key) |
         key <- keys, lock <- locks, and (zipWith (<=) key lock)]

testInput :: String
testInput = "\
    \#####\n\
    \.####\n\
    \.####\n\
    \.####\n\
    \.#.#.\n\
    \.#...\n\
    \.....\n\
    \\n\
    \#####\n\
    \##.##\n\
    \.#.##\n\
    \...##\n\
    \...#.\n\
    \...#.\n\
    \.....\n\
    \\n\
    \.....\n\
    \#....\n\
    \#....\n\
    \#...#\n\
    \#.#.#\n\
    \#.###\n\
    \#####\n\
    \\n\
    \.....\n\
    \.....\n\
    \#.#..\n\
    \###..\n\
    \###.#\n\
    \###.#\n\
    \#####\n\
    \\n\
    \.....\n\
    \.....\n\
    \.....\n\
    \#....\n\
    \#.#..\n\
    \#.#.#\n\
    \#####\n\
    \"

tests1 :: [(String, Int)]
tests1 = [(testInput, 3)]

-- no Part Two on day 25

main :: IO ()
main = do
    s <- readFile "input/25.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
