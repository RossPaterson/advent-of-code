module Main where

import Geometry
import Graph
import Utilities
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Input processing

data Device = LeftMirror | RightMirror | HorizSplitter | VertSplitter
    deriving (Show)

type Input = (AABox Position, Map Position Device)

parse :: String -> Input
parse s = (box, devices)
  where
    pcs = readGrid s
    box = boundingBox (map fst pcs)
    devices =
        Map.fromList [(p, d) | (p, c) <- pcs, d <- maybeToList (getDevice c)]

getDevice :: Char -> Maybe Device
getDevice '/' = Just LeftMirror
getDevice '\\' = Just RightMirror
getDevice '-' = Just HorizSplitter
getDevice '|' = Just VertSplitter
getDevice _ = Nothing

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

type Beam = (Position, Direction)

-- start from the top left corner
startBeam :: Beam
startBeam = (zero, E)

-- one step in moving a beam
move :: AABox Position -> Map Position Device -> Beam -> [Beam]
move box devices (p, d) =
    filter ((`inBox` box) . fst) $ map advance $
    [(p, d') | d' <- maybe [d] (bounce d) (Map.lookup p devices)]

-- bounce a beam direction off a mirror or splitter
bounce :: Direction -> Device -> [Direction]
bounce N LeftMirror = [E]
bounce S LeftMirror = [W]
bounce E LeftMirror = [N]
bounce W LeftMirror = [S]
bounce N RightMirror = [W]
bounce S RightMirror = [E]
bounce E RightMirror = [S]
bounce W RightMirror = [N]
bounce N HorizSplitter = [E, W]
bounce S HorizSplitter = [E, W]
bounce E VertSplitter = [N, S]
bounce W VertSplitter = [N, S]
bounce d _ = [d]

advance :: Beam -> Beam
advance (p, d) = (p .+. oneStep d, d)

-- number of energized positions from starting beam
energized :: AABox Position -> Map Position Device -> Beam -> Int
energized box devices beam =
    Set.size $ Set.fromList $ map fst $ concat $ bfs (move box devices) [beam]

solve1 :: Input -> Int
solve1 (box, devices) = energized box devices startBeam

testInput :: String
testInput = "\
    \.|...\\....\n\
    \|.-.\\.....\n\
    \.....|-...\n\
    \........|.\n\
    \..........\n\
    \.........\\\n\
    \..../.\\\\..\n\
    \.-.-/..|..\n\
    \.|....-|.\\\n\
    \..//.|....\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 46)]

-- Part Two

-- beams starting from all the edge positions of the box
startBeams :: AABox Position -> [Beam]
startBeams box =
    [(Position x 0, S) | x <- [0..max_x]] ++
    [(Position x max_y, N) | x <- [0..max_x]] ++
    [(Position 0 y, E) | y <- [0..max_y]] ++
    [(Position max_x y, W) | y <- [0..max_y]]
  where
    Position max_x max_y = maxCorner box

solve2 :: Input -> Int
solve2 (box, devices) =
    maximum [energized box devices beam | beam <- startBeams box]

tests2 :: [(String, Int)]
tests2 = [(testInput, 51)]

main :: IO ()
main = do
    s <- readFile "input/16.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
