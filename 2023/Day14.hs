module Main where

import Geometry
import Utilities
import qualified Data.CyclicList as CL
import Data.Set (Set)
import qualified Data.Set as Set

-- Input processing

-- statis platform and changing set of positions of rounded rocks
type Input = (Platform, Set Position)
-- fixed positions and whole area
data Platform = Platform (Set Position) (AABox Position)
    deriving (Show)

parse :: String -> Input
parse s = (Platform (get '#') (boundingBox (map fst pcs)), get 'O')
  where
    pcs = readGrid s
    get x = Set.fromList [p | (p, c) <- pcs, c == x]

showState :: Platform -> Set Position -> String
showState (Platform fixed box) ps = showBox box showPos
  where
    showPos p
      | Set.member p fixed = '#'
      | Set.member p ps = 'O'
      | otherwise = '.'

-- Part One

-- Directions

data Direction = N | E | S | W
    deriving (Bounded, Enum, Eq, Ord, Show)

-- one position in the direction
oneStep :: Direction -> Position
oneStep N = Position 0 (-1)
oneStep E = Position 1 0
oneStep S = Position 0 1
oneStep W = Position (-1) 0

-- tilt the platform in direction d
tilt :: Platform -> Set Position -> Direction -> Set Position
tilt platform ps d = Set.fromList (concatMap expand groups)
  where
    -- resting points and number of rocks heading for each
    groups = frequency (map (destination platform d) (Set.elems ps))
    delta = oneStep d
    -- expand a group of rocks heading for p as a stack
    expand (p, n) = take n (iterate (.-. delta) p)

-- where a rounded rock at p would roll to on a tilt in direction d
-- if there were no other rounded rocks
destination :: Platform -> Direction -> Position -> Position
destination (Platform fixed box) d =
    last . takeWhile possible . iterate (.+. delta)
  where
    possible p = inBox p box && not (Set.member p fixed)
    delta = oneStep d

-- summary function
load :: Platform -> Set Position -> Int
load (Platform _ box) ps =
    sum [max_y - y + 1 | Position _ y <- Set.elems ps]
  where
    Position _ max_y = maxCorner box

solve1 :: Input -> Int
solve1 (platform, ps) = load platform (tilt platform ps N)

testInput :: String
testInput = "\
    \O....#....\n\
    \O.OO#....#\n\
    \.....##...\n\
    \OO.#O....O\n\
    \.O.....O#.\n\
    \O.#..O.#.#\n\
    \..O..#O..O\n\
    \.......O..\n\
    \#....###..\n\
    \#OO..#....\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 136)]

-- Part Two

-- tilt the platform N, then W, then S, then E
spin_cycle :: Platform -> Set Position -> Set Position
spin_cycle platform ps = foldl (tilt platform) ps [N, W, S, E]

-- Assume that the system cycles after a while, because otherwise the
-- problem is infeasible.

solve2 :: Input -> Int
solve2 (platform, ps) =
    load platform $
        CL.elementAt 1000000000 $ CL.iterate (spin_cycle platform) ps

tests2 :: [(String, Int)]
tests2 = [(testInput, 64)]

main :: IO ()
main = do
    s <- readFile "input/14.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
