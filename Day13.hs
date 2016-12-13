module Day13 where

import Utilities
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
    length $ takeWhile (not . elem goal) $ bfs (neighbours magic) start

test :: IO ()
test = print (solve 10 (7, 4))

input :: Int
input = 1362

puzzle1 :: IO ()
puzzle1 = print (solve input (31, 39))

puzzle2 :: IO ()
puzzle2 = print $ sum $ map length $ take 51 $ bfs (neighbours input) start
