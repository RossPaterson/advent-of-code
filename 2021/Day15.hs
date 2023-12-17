module Main where

import Utilities
import Geometry
import Graph
import Data.Char
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

-- Input processing

type Grid = Map Position Int
type Input = Grid

parse :: String -> Input
parse s = Map.fromList [(p, digitToInt c) | (p, c) <- readGrid s]

-- Part One

start :: Grid -> Position
start = fst . Map.findMin

goal :: Grid -> Position
goal = fst . Map.findMax

neighbours :: Grid -> Position -> [(Int, Position)]
neighbours g p = [(v, p') |
    d <- unitVectors, let p' = p .+. d, v <- maybeToList (Map.lookup p' g)]

solve1 :: Input -> Int
solve1 g =
    head [d | (d, p) <- shortestPaths (neighbours g) [start g], p == finish]
  where
    finish = goal g

testInput :: String
testInput = "\
    \1163751742\n\
    \1381373672\n\
    \2136511328\n\
    \3694931569\n\
    \7463417111\n\
    \1319128137\n\
    \1359912421\n\
    \3125421639\n\
    \1293138521\n\
    \2311944581\n\
    \"

tests1 :: [(String, Int)]
tests1 = [(testInput, 40)]

-- Part Two

-- wrap values above 9 around to 1
wrap :: Int -> Int
wrap n = (n - 1) `mod` 9 + 1

-- grow grid by a factor of n in each direction
grow :: Int -> Grid -> Grid
grow n g =
    Map.fromList [(Position (x + i*w) (y + j*h), wrap (v + i + j)) |
        (Position x y, v) <- Map.assocs g, i <- [0..n-1], j <- [0..n-1]]
  where
    Position xmax ymax = fst (Map.findMax g)
    w = xmax + 1
    h = ymax + 1

solve2 :: Input -> Int
solve2 = solve1 . grow 5

tests2 :: [(String, Int)]
tests2 = [(testInput, 315)]

main :: IO ()
main = do
    s <- readFile "input/15.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
