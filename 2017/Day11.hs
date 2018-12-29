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
-- (= axial coordinate at https://www.redblobgames.com/grids/hexagons/)
data Coord = Coord Int Int
    deriving (Show)

origin :: Coord
origin = Coord 0 0

add :: Coord -> Coord -> Coord
add (Coord x1 y1) (Coord x2 y2) = Coord (x1+x2) (y1+y2)

-- one step in the skew coordinate system:
--
--   NW | N |
--   SW | * | NE
--      | S | SE
--
direction :: Direction -> Coord
direction N  = Coord 0 1
direction NE = Coord 1 0
direction SE = Coord 1 (-1)
direction S  = Coord 0 (-1)
direction SW = Coord (-1) 0
direction NW = Coord (-1) 1

-- number of steps from the origin
metric :: Coord -> Int
metric (Coord x y) =  maximum [abs x, abs y, abs (x+y)]

solve1 :: Input -> Int
solve1 = metric . foldl add origin . map direction

tests1 :: [(String, Int)]
tests1 = [
    ("ne,ne,ne", 3),
    ("ne,ne,sw,sw", 0),
    ("ne,ne,s,s", 2),
    ("se,sw,se,sw,sw", 3)]

-- Part Two

solve2 :: Input -> Int
solve2 = maximum . map metric . scanl add origin . map direction

main :: IO ()
main = do
    s <- readFile "input/11.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    print (solve2 input)
