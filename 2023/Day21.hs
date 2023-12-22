module Main where

import Geometry
import Graph
import Utilities
import Data.Set (Set)
import qualified Data.Set as Set

-- Input processing

-- square area
data Garden = Garden {
    size :: Int,
    startPos :: Position,
    rocks :: Set Position
    }
    deriving (Show)

type Input = Garden

parse :: String -> Input
parse s = Garden {
    size = length (lines s),
    startPos = head [p | (p, c) <- pcs, c == 'S'],
    rocks = Set.fromList [p | (p, c) <- pcs, c == '#']
    }
  where
    pcs = readGrid s

-- Part One

-- neighbouring positions within the set
neighbours :: AABox Position -> Set Position -> Position -> [Position]
neighbours box s p =
    [p' | delta <- unitVectors,
        let p' = p .+. delta, inBox p' box, not (Set.member p' s)]

-- positions in the garden reachable in exactly n steps
destinations :: Int -> Garden -> Int
destinations n g =
    sumReach n $ map length $ bfs (neighbours box (rocks g)) [startPos g]
  where
    max_xy = size g - 1
    box = boundingBox [zero, Position max_xy max_xy]

-- positions reachable in exactly n steps
sumReach :: Int -> [Int] -> Int
sumReach n xs =
    sum [x | (k, x) <- zip [0..] (take (n+1) xs), k `mod` 2 == parity]
  where
    parity = n `mod` 2

solve1 :: Input -> Int
solve1 = destinations 64

testInput :: String
testInput = "\
    \...........\n\
    \.....###.#.\n\
    \.###.##..#.\n\
    \..#.#...#..\n\
    \....#.#....\n\
    \.##..S####.\n\
    \.##..#...#.\n\
    \.......##..\n\
    \.##.#.####.\n\
    \.##..##.##.\n\
    \...........\n"

tests1 :: [((Int, String), Int)]
tests1 = [((6, testInput), 16)]

-- Part Two

-- Naive implementation with the repeating garden,
-- used as a check against the real implementation

-- positions in the garden reachable in exactly n steps
destinationsRepeating :: Int -> Garden -> Int
destinationsRepeating n g =
    sumReach n $ map length $ bfs (neighboursRepeating g) [startPos g]

-- neighbouring positions within infinite repetitions of the garden
neighboursRepeating :: Garden -> Position -> [Position]
neighboursRepeating g p =
    [p' | delta <- unitVectors,
        let p'@(Position x y) = p .+. delta,
        not (Set.member (Position (x `mod` w) (y `mod` w)) (rocks g))]
  where
    w = size g

{-
The actual input has some simplifying properties:
(1) The garden is square, with an odd side
(2) The start point is the centre of the square
(3) The column and row of the start point contain no rocks
    (not true of the sample input)

Together, these imply that we can use the repetitions of the start point
as waypoints, because they lie on a square lattice with shortest paths
between repetitions of the start node following the lattice.

    ...S...S...S...
       |   |   |
    ...S...S...S...
       |   |   |
    ...S...S...S...

If we look at the squares NW, NE, SE and SW of the original start point,
and work out how many of their cells are reachable from S in each number
of steps, we can repeat them as far as we can reach, building a triangle
of these squares in each quadrant.

We include the edges of each square closest to the original start point
to get complete coverage, but that means we double-count positions on
the axes and count the origin 4 times, so we have to correct for that.

We could speed this up by noting that a huge diamond of squares are fully
reachable, and there are formulas to count them, but this is fast enough.
-}

-- Implementation that only works for square grids where the column and
-- row of the start point contain no rocks (as in the real input, but not
-- the sample input)

-- positions in the repeating garden reachable in exactly n steps
destinationsSpecial :: Int -> Input -> Int
destinationsSpecial n g = repeats - double_counting
  where
    w = size g
    fromAnyCorner = anyCornerDists g
    -- Moving outwards by w adds 1, 2, ... identical squares to each
    -- quadrant, building a blunt diamond of lattice squares.
    repeats = sum [k*(sumReach n' fromAnyCorner) |
        (k, n') <- zip [1..] $ takeWhile (>= 0) $ iterate (subtract w) n]
    -- The origin is counted 4 times, and other positions on the axes
    -- are counted twice each.
    double_counting
      | odd n = 2*(n+1)
      | otherwise = 2*n + 3

-- sum of the numbers reachable in exact number of steps for the 4
-- lattice squares adjacent to the start point
anyCornerDists :: Garden -> [Int]
anyCornerDists g =
    foldr1 (longZipWith (+)) [cornerDists g dp | dp <- corners]

-- number reachable in exact number of steps for a lattice square at
-- the given corner from the start point
cornerDists :: Garden -> Position -> [Int]
cornerDists g dp =
    map (length . filter (`inBox` box)) $
        bfs (filter (`inBox` big_box) . neighboursRepeating g) [start]
  where
    w = size g
    start = startPos g
    -- Two boxes are needed, because the shortest path to a cell in the
    -- box of interest may go through the corridor just outside.
    box = boundingBox [start, start .+. (w-1) *. dp]
    big_box = boundingBox [start, start .+. w *. dp]

solve2 :: Input -> Int
solve2 = destinationsSpecial 26501365

tests2 :: [((Int, String), Int)]
tests2 = [
    ((6, testInput), 16), ((10, testInput), 50), ((50, testInput), 1594),
    ((100, testInput), 6536), ((500, testInput), 167004)]
    -- These take too long with the naive implementation.
    -- ((1000, testInput), 668697), ((5000, testInput), 16733044)]

main :: IO ()
main = do
    s <- readFile "input/21.txt"
    let input = parse s
    putStr (unlines (failures "destinations" (\(n, t) -> destinations n (parse t)) tests1))
    print (solve1 input)
    putStr (unlines (failures "destinationsRepeating" (\(n, t) -> destinationsRepeating n (parse t)) tests2))
    print (solve2 input)
