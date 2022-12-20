module Main where

import Utilities

-- Input processing

type Input = [Int]

parse :: String -> Input
parse = map read . lines

-- Part One

-- a value with its original position
data Located = Loc { orig_pos :: !Int, value :: !Int }
    deriving (Show)

addlocs :: [Int] -> [Located]
addlocs = zipWith Loc [1..]

-- Move one element forward or backward by its value.
-- n is the length of the list without the moving element
-- i is the original position of the element to be moved
move :: Int -> [Located] -> Int -> [Located]
move n ps i = front' ++ p:back'
  where
    (front, p:back) = span ((/= i) . orig_pos) ps
    delta = value p `mod` n
    (front', back') = splitAt delta (back ++ front)

-- Move each element in order of its original position.
mix :: [Located] -> [Located]
mix xs = foldl (move (length xs-1)) xs [1..length xs]

summary :: [Int] -> Int
summary xs = sum [list!!(i `mod` n) | i <- [1000, 2000, 3000]]
  where
    (front, back) = span (/= 0) xs
    list = back++front
    n = length list

solve1 :: Input -> Int
solve1 = summary . map value . mix . addlocs

testInput :: String
testInput = "\
    \1\n\
    \2\n\
    \-3\n\
    \3\n\
    \-2\n\
    \0\n\
    \4\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 3)]

-- Part Two

key :: Int
key = 811589153

solve2 :: Input -> Int
solve2 = summary . map value . times 10 mix . addlocs . map (*key)

tests2 :: [(String, Int)]
tests2 = [(testInput, 1623178306)]

main :: IO ()
main = do
    s <- readFile "input/20.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
