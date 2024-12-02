module Main where

import Utilities

-- Input processing

type Input = [[Int]]

parse :: String -> Input
parse s = [map read (words l) | l <- lines s]

-- Part One

solve1 :: Input -> Int
solve1 = length . filter safe

safe :: [Int] -> Bool
safe ns = adjacent gradualIncrease ns || adjacent (flip gradualIncrease) ns

-- all adjacent pairs in the list have the relationship
adjacent :: (a -> a -> Bool) -> [a] -> Bool
adjacent _ [] = True
adjacent rel (x:xs) = and (zipWith rel xs (x:xs))

gradualIncrease :: Int -> Int -> Bool
gradualIncrease x y = x+1 <= y && y <= x+3

testInput :: String
testInput = "\
    \7 6 4 2 1\n\
    \1 2 7 8 9\n\
    \9 7 6 2 1\n\
    \1 3 2 4 5\n\
    \8 6 4 4 1\n\
    \1 3 6 7 9\n\
    \"

tests1 :: [(String, Int)]
tests1 = [(testInput, 2)]

-- Part Two

solve2 :: Input -> Int
solve2 = length . filter safe2

safe2 :: [Int] -> Bool
safe2 ns = safe ns || any safe (withoutOne ns)

-- ways of removing one element from a list
withoutOne :: [a] -> [[a]]
withoutOne xs = [front++back | (front, _:back) <- splits xs]

tests2 :: [(String, Int)]
tests2 = [(testInput, 4)]

main :: IO ()
main = do
    s <- readFile "input/02.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
