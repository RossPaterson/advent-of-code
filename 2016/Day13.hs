module Main where

import Utilities
import Graph
import Data.Bits

type Point = (Int, Int)

start :: Point
start = (1, 1)

openSpace :: Int -> Point -> Bool
openSpace magic (x, y) =
    x >= 0 && y >= 0 && even (popCount (x*x + 3*x + 2*x*y + y + y*y + magic))

neighbours :: Int -> Point -> [Point]
neighbours magic (x, y) =
    filter (openSpace magic) [(x-1, y), (x, y-1), (x+1, y), (x, y+1)]

example = unlines
    [[if openSpace 10 (x, y) then '.' else '#' | x <- [0..9]] | y <- [0..9]]

solve :: Int -> Point -> Int
solve magic goal =
    length $ takeWhile (not . elem goal) $ bfs (neighbours magic) [start]

test :: IO ()
test = print (solve 10 (7, 4))

input :: Int
input = 1362

-- Part Two

solve2 :: Int -> Int -> Int
solve2 magic depth =
    sum $ map length $ take (depth+1) $ bfs (neighbours magic) [start]

main :: IO ()
main = do
    print (solve input (31, 39))
    print (solve2 input 50)
