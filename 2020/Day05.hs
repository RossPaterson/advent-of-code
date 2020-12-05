module Main where

import Utilities
import qualified Data.Set as Set

-- Input processing

type Input = [Int]

parse :: String -> Input
parse = map seatID . lines

-- more significant 7 bits (row) specified by letters F = 0, B = 1
-- less significant 3 bits (column) specified by letters L = 0, R = 1
seatID :: String -> Int
seatID = fromBits . map (flip elem "BR")

-- number represented by a list of bits, most significant first
fromBits :: [Bool] -> Int
fromBits bs = sum [v | (b, v) <- zip (reverse bs) (iterate (*2) 1), b]

-- Part One

solve1 :: Input -> Int
solve1 = maximum

tests1 :: [(String, Int)]
tests1 = [
    ("FBFBBFFRLR", 357),
    ("BFFFBBFRRR", 567),
    ("FFFBBBFRRR", 119),
    ("BBFFBBFRLL", 820)]

-- Part Two

-- intermediate values not present in the list
missing :: (Enum a, Ord a) => [a] -> [a]
missing xs =
    Set.elems (Set.difference (Set.fromList fullRange) (Set.fromList xs))
  where
    fullRange = [minimum xs..maximum xs]

solve2 :: Input -> Int
solve2 = head . missing

main :: IO ()
main = do
    s <- readFile "input/05.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    print (solve2 input)
