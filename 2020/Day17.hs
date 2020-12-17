module Main where

import Geometry
import Utilities
import Data.Set (Set)
import qualified Data.Set as Set

-- Input processing

type Input = Set Point3

parse :: String -> Input
parse s = Set.fromList
    [Point3 col (-row) 0 | (Position col row, c) <- readGrid s, c == '#']

-- Part One

-- the 26 neighbours of a point in 3-dimensional space
neighbours3 :: Point3 -> Set Point3
neighbours3 (Point3 x y z) =
    Set.fromList [Point3 (x+dx) (y+dy) (z+dz) |
        dx <- [-1..1], dy <- [-1..1], dz <- [-1..1],
        not (dx == 0 && dy == 0 && dz == 0)]

-- the rule from John Conway's Game of Life
conway_rule :: Bool -> Int -> Bool
conway_rule True n = n == 2 || n == 3
conway_rule False n = n == 3

-- The next generation of a cellular automaton (modelled by a set of
-- cells), determined by a neighbours function and a reproduction rule.
generation :: Ord a => (a -> Set a) -> (Bool -> Int -> Bool) -> Set a -> Set a
generation neighbours rule s = Set.filter alive candidates
  where
    candidates = Set.union s (Set.unions (map neighbours (Set.elems s)))
    alive c = rule (Set.member c s)
        (Set.size (Set.intersection s (neighbours c)))

solve1 :: Input -> Int
solve1 = Set.size . times 6 (generation neighbours3 conway_rule)

testInput :: String
testInput = "\
    \.#.\n\
    \..#\n\
    \###\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 112)]

-- Part Two

promote :: Set Point3 -> Set Point4
promote s = Set.fromList [Point4 x y z 0 | Point3 x y z <- Set.elems s]

-- the 80 neighbours of a point in 4-dimensional space
neighbours4 :: Point4 -> Set Point4
neighbours4 (Point4 x y z w) =
    Set.fromList [Point4 (x+dx) (y+dy) (z+dz) (w+dw) |
        dx <- [-1..1], dy <- [-1..1], dz <- [-1..1], dw <- [-1..1],
        not (dx == 0 && dy == 0 && dz == 0 && dw == 0)]

solve2 :: Input -> Int
solve2 = Set.size . times 6 (generation neighbours4 conway_rule) . promote

tests2 :: [(String, Int)]
tests2 = [(testInput, 848)]

main :: IO ()
main = do
    s <- readFile "input/17.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
