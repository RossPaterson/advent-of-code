module Main where

import Utilities
import Data.Char

-- directions in an infinite hexagonal tiling
data Direction = N | NE | SE | S | SW | NW
    deriving (Read, Show)

type Input = [Direction]

parse :: String -> Input
parse cs = map read $
    words [if c == ',' then ' ' else toUpper c | c <- cs, c /= '\n']

-- coordinate in an infinite hexagonal tiling: x+y is even
data Coord = Coord Int Int
    deriving (Eq, Show)

start :: Coord
start = Coord 0 0

-- metric (step start d) = 2
step :: Coord -> Direction -> Coord
step (Coord x y) N  = Coord x (y+2)
step (Coord x y) NE = Coord (x+1) (y+1)
step (Coord x y) SE = Coord (x+1) (y-1)
step (Coord x y) S  = Coord x (y-2)
step (Coord x y) SW = Coord (x-1) (y-1)
step (Coord x y) NW = Coord (x-1) (y+1)

metric :: Coord -> Int
metric (Coord x y) = (abs x + abs y) `div` 2

solve1 :: Input -> Int
solve1 = metric . foldl step start

tests1 :: [(String, Int)]
tests1 = [
    ("ne,ne,ne", 3),
    ("ne,ne,sw,sw", 0),
    ("ne,ne,s,s", 2),
    ("se,sw,se,sw,sw", 3)]

-- Part Two

solve2 :: Input -> Int
solve2 = maximum . map metric . scanl step start

main :: IO ()
main = do
    s <- readFile "input11.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    print (solve2 input)
