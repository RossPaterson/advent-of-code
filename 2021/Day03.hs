module Main where

import Utilities
import Data.List

-- Input processing

type Input = [Bits]
type Bits = [Bool]

parse :: String -> Input
parse = map (map (== '1')) . lines

bitsToInt :: Bits -> Int
bitsToInt = foldl add 0 . map fromEnum
  where
    add n b = n*2 + b

-- Part One

-- most common bit, choosing 1 on tie
common :: Bits -> Bool
common bs = length (filter id bs) >= length (filter not bs)

-- select each element independently
select1 :: Eq a => ([a] -> a) -> [[a]] -> [a]
select1 criterion = map criterion . transpose

solve1 :: Input -> Int
solve1 bs =
    bitsToInt (select1 common bs) * bitsToInt (select1 (not . common) bs)

testInput :: String
testInput = "\
    \00100\n\
    \11110\n\
    \10110\n\
    \10111\n\
    \10101\n\
    \01111\n\
    \00111\n\
    \11100\n\
    \10000\n\
    \11001\n\
    \00010\n\
    \01010\n\
    \"

tests1 :: [(String, Int)]
tests1 = [(testInput, 198)]

-- Part Two

select2 :: Eq a => ([a] -> a) -> [[a]] -> [a]
select2 _ [] = error "empty"
select2 _ [x] = x
select2 criterion xs =
    c:select2 criterion (map tail (filter ((== c) . head) xs))
  where
    c = criterion (map head xs)

solve2 :: Input -> Int
solve2 bs =
    bitsToInt (select2 common bs) * bitsToInt (select2 (not . common) bs)

tests2 :: [(String, Int)]
tests2 = [(testInput, 230)]

main :: IO ()
main = do
    s <- readFile "input/03.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
