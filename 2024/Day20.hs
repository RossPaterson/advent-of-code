module Main where

import Geometry
import Graph
import Utilities
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

-- Input processing

type Input = Maze
type Maze = (Position, Position, Set Position)

parse :: String -> Input
parse s = (start, finish, points)
  where
    start = head [p | (p, c) <- pcs, c == 'S']
    finish = head [p | (p, c) <- pcs, c == 'E']
    points = Set.fromList [p | (p, c) <- pcs, c /= '#']
    pcs = readGrid s

-- Part One

solve1 :: Input -> Int
solve1 = length . filter (>= 100) . map snd . cheats 2

-- Cheat: walk from p1 to p2 ignoring walls
type Cheat = (Position, Position)

-- List of possible cheats no more than len apart, with resulting savings,
-- assuming a single path from the start to the finish.
-- A cheat (p1, p2) allows one to walk from p1 to p2 ignoring walls.
-- The time to reach p2 using this cheat is then
--
--      start_to_p1 + distance p1 p2
--
-- We then subtract this from start_to_p2 to see whether it produces
-- a saving.
cheats :: Int -> Maze -> [(Cheat, Int)]
cheats len (start, _, open) =
    [((p1, p2), saving) |
        (start_to_p1, p1):rest <- tails from_start,
        (start_to_p2, p2) <- rest,
        let p1_to_p2 = distance p1 p2, p1_to_p2 <= len,
        let saving = start_to_p2 - start_to_p1 - p1_to_p2, saving > 0]
  where
    -- nodes with distances from the start
    from_start = zip [0..] (map head (bfs (moves open) [start]))

-- possible moves to an open position
moves :: Set Position -> Position -> [Position]
moves open p =
    [p' | d <- unitVectors, let p' = p .+. d, Set.member p' open]

-- number of cheats providing a given saving (for testing)
cheatFreq :: Int -> Maze -> [(Int, Int)]
cheatFreq n = frequency . map snd . cheats n

testInput :: String
testInput = "\
    \###############\n\
    \#...#...#.....#\n\
    \#.#.#.#.#.###.#\n\
    \#S#...#.#.#...#\n\
    \#######.#.#.###\n\
    \#######.#.#...#\n\
    \#######.#.###.#\n\
    \###..E#...#...#\n\
    \###.#######.###\n\
    \#...###...#...#\n\
    \#.#####.#.###.#\n\
    \#.#...#.#.#...#\n\
    \#.#.#.#.#.#.###\n\
    \#...#...#...###\n\
    \###############\n\
    \"

tests1 :: [(String, [(Int, Int)])]
tests1 = [(testInput,
    [(2,14),(4,14),(6,2),(8,4),(10,2),(12,3),
     (20,1),(36,1),(38,1),(40,1),(64,1)])]

-- Part Two

solve2 :: Input -> Int
solve2 = length . filter (>= 100) . map snd . cheats 20

tests2 :: [(String, [(Int, Int)])]
tests2 = [(testInput,
    [(50,32), (52,31), (54,29), (56,39), (58,25), (60,23), (62,20),
     (64,19), (66,12), (68,14), (70,12), (72,22), (74,4), (76,3)])]

-- 123135 540027 wrong

main :: IO ()
main = do
    s <- readFile "input/20.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (cheatFreq 2 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (filter ((>= 50) . fst) . cheatFreq 20 . parse) tests2))
    print (solve2 input)
