module Main where

import Cartesian
import Graph
import Data.Bits

start :: Position
start = Position 1 1

openSpace :: Int -> Position -> Bool
openSpace magic (Position x y) =
    x >= 0 && y >= 0 && even (popCount (x*x + 3*x + 2*x*y + y + y*y + magic))

neighbours :: Int -> Position -> [Position]
neighbours magic (Position x y) =
    filter (openSpace magic)
        [Position (x-1) y, Position x (y-1), Position (x+1) y, Position x (y+1)]

example :: String
example = unlines
    [[if openSpace 10 (Position x y) then '.' else '#' |
        x <- [0..9]] | y <- [0..9]]

solve :: Int -> Position -> Int
solve magic goal =
    length $ takeWhile (not . elem goal) $ bfs (neighbours magic) [start]

test :: IO ()
test = print (solve 10 (Position 7 4))

input :: Int
input = 1362

-- Part Two

solve2 :: Int -> Int -> Int
solve2 magic depth =
    sum $ map length $ take (depth+1) $ bfs (neighbours magic) [start]

main :: IO ()
main = do
    print (solve input (Position 31 39))
    print (solve2 input 50)
