module Main where

import Utilities
import Data.Char

-- directions in an infinite hexagonal tiling
data Direction = N | NE | SE | S | SW | NW
    deriving (Read, Show)

type Input = [Direction]

parse :: String -> Input
parse cs = read ("[" ++ map toUpper cs ++ "]")

-- skew coordinate in an infinite hexagonal tiling
data Coord = Coord Int Int
    deriving (Eq, Show)

start :: Coord
start = Coord 0 0

-- one step in the skey coordinate system:
--
--   NW | N |
--   SW |   | NE
--      | S | SE
--
step :: Coord -> Direction -> Coord
step (Coord x y) N  = Coord x (y+1)
step (Coord x y) NE = Coord (x+1) y
step (Coord x y) SE = Coord (x+1) (y-1)
step (Coord x y) S  = Coord x (y-1)
step (Coord x y) SW = Coord (x-1) y
step (Coord x y) NW = Coord (x-1) (y+1)

-- number o steps from the start
metric :: Coord -> Int
metric (Coord x y) =  maximum [abs x, abs y, abs (x+y)]

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
