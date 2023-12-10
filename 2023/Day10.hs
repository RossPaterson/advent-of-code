module Main where

import Geometry
import Utilities
import Data.List
import Data.Maybe
import Data.Map (Map, (!))
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Input processing

-- Directions

data Direction = N | E | S | W
    deriving (Bounded, Enum, Eq, Ord, Show)

-- the opposite direction
opposite :: Direction -> Direction
opposite N = S
opposite E = W
opposite S = N
opposite W = E

-- one position in the direction
oneStep :: Direction -> Position
oneStep N = Position 0 (-1)
oneStep E = Position 1 0
oneStep S = Position 0 1
oneStep W = Position (-1) 0

-- Pipe pieces

data Piece = NS | EW | NE | NW | SW | SE
    deriving (Bounded, Enum, Eq, Ord, Show)

read_piece :: Char -> Maybe Piece
read_piece '|' = Just NS
read_piece '-' = Just EW
read_piece 'L' = Just NE
read_piece 'J' = Just NW
read_piece '7' = Just SW
read_piece 'F' = Just SE
read_piece _ = Nothing

-- the directions of the ends of this pipe piece, in sorted order
ends :: Piece -> [Direction]
ends NS = [N, S]
ends EW = [E, W]
ends NE = [N, E]
ends NW = [N, W]
ends SW = [S, W]
ends SE = [E, S]

-- the piece (if any) with ends in the given directions
piece_with :: [Direction] -> Maybe Piece
piece_with ds = listToMaybe [p | p <- allValues, ends p == ds']
  where
    ds' = sort ds

type Pipes = Map Position Piece

-- Guess what pipe piece is at position p, given the rest of the map and
-- assuming that p is connected to exactly two of its neighbours.
piece_at :: Pipes -> Position -> Piece
piece_at pieces p =
    fromMaybe (error "bad start") $ piece_with
        [d |
            d <- allValues,
            piece <- maybeToList (Map.lookup (p .+. oneStep d) pieces),
            elem (opposite d) (ends piece)]

type Input = (Position, Pipes)

parse :: String -> Input
parse s = (start, all_pieces)
  where
    marked_pieces =
        Map.fromList [(p, piece) |
            (p, c) <- readGrid s,
            piece <- maybeToList (read_piece c)]
    start = head [p | (p, c) <- readGrid s, c == 'S']
    start_piece = piece_at marked_pieces start
    all_pieces = Map.insert start start_piece marked_pieces

-- Part One

-- Make a single move through the pipe from a position and facing direction.
step :: Pipes -> (Position, Direction) -> (Position, Direction)
step pieces (p, d) = (p', d')
  where
    p' = p .+. oneStep d
    ds = ends (pieces!p')
    d' = head (delete (opposite d) ds)

-- Assuming that the start point is on a cycle of positions, return the
-- list of positions in the cycle.
loop :: Pipes -> Position -> [Position]
loop pieces start =
    map fst (takeWhile (/= s) (tail (iterate (step pieces) s))) ++ [start]
  where
    s = (start, head (ends (pieces!start)))

-- Furtherest distance: the loop must be of even length
solve1 :: Input -> Int
solve1 (start, pieces) = length (loop pieces start) `div` 2

testInput1 :: String
testInput1 = "\
    \.....\n\
    \.S-7.\n\
    \.|.|.\n\
    \.L-J.\n\
    \.....\n"

testInput2 :: String
testInput2 = "\
    \-L|F7\n\
    \7S-7|\n\
    \L|7||\n\
    \-L-J|\n\
    \L|-JF\n"

testInput3 :: String
testInput3 = "\
    \..F7.\n\
    \.FJ|.\n\
    \SJ.L7\n\
    \|F--J\n\
    \LJ...\n"

testInput4 :: String
testInput4 = "\
    \7-F7-\n\
    \.FJ|7\n\
    \SJLL7\n\
    \|F--J\n\
    \LJ.LJ\n"

tests1 :: [(String, Int)]
tests1 = [(testInput1, 4), (testInput2, 4), (testInput3, 8), (testInput4, 8)]

-- Part Two

-- A position is inside the loop if is not on the loop and any path to
-- the outside crosses the loop an odd number of times.  We use a linear
-- path extending east from the position.
interior :: Map Position Piece -> Int
interior loop_pieces =
    length $ filter inside $ filter not_on_loop $ boxElements box
  where
    box = boundingBox (Map.keys loop_pieces)
    Position max_x _ = maxCorner box
    not_on_loop p = not (Map.member p loop_pieces)
    -- linear path east from p to the edge of the bounding box
    out_path (Position x y) = [Position x' y | x' <- [x..max_x]]
    -- pipe pieces on that path, excluding east-west pieces
    out_pieces p = filter (/= EW) $
        catMaybes [Map.lookup p' loop_pieces | p' <- out_path p]
    inside p = odd (crossings (out_pieces p))

-- the number of times an east-wards path crosses the loop,
-- given the pices along the path, excluding east-west pieces
crossings :: [Piece] -> Int
crossings [] = 0
-- connecting north and south makes one enclosing edge
crossings (NS:cs) = 1 + crossings cs
crossings (NE:SW:cs) = 1 + crossings cs
crossings (SE:NW:cs) = 1 + crossings cs
-- U-bends do not enclose anything
crossings (NE:NW:cs) = crossings cs
crossings (SE:SW:cs) = crossings cs
crossings _ = error "bad loop"

-- the loop, with the pieces at each position
loop_with_pieces :: Pipes -> Position -> Map Position Piece
loop_with_pieces pieces start =
    Map.restrictKeys pieces $ Set.fromList $ loop pieces start

-- number of positions inside the loop
solve2 :: Input -> Int
solve2 (start, pieces) = interior (loop_with_pieces pieces start)

testInput5 :: String
testInput5 = "\
    \...........\n\
    \.S-------7.\n\
    \.|F-----7|.\n\
    \.||.....||.\n\
    \.||.....||.\n\
    \.|L-7.F-J|.\n\
    \.|..|.|..|.\n\
    \.L--J.L--J.\n\
    \...........\n"

testInput6 :: String
testInput6 = "\
    \.F----7F7F7F7F-7....\n\
    \.|F--7||||||||FJ....\n\
    \.||.FJ||||||||L7....\n\
    \FJL7L7LJLJ||LJ.L-7..\n\
    \L--J.L7...LJS7F-7L7.\n\
    \....F-J..F7FJ|L7L7L7\n\
    \....L7.F7||L7|.L7L7|\n\
    \.....|FJLJ|FJ|F7|.LJ\n\
    \....FJL-7.||.||||...\n\
    \....L---J.LJ.LJLJ...\n"

testInput7 :: String
testInput7 = "\
    \FF7FSF7F7F7F7F7F---7\n\
    \L|LJ||||||||||||F--J\n\
    \FL-7LJLJ||||||LJL-77\n\
    \F--JF--7||LJLJ7F7FJ-\n\
    \L---JF-JLJ.||-FJLJJ7\n\
    \|F|F-JF---7F7-L7L|7|\n\
    \|FFJF7L7F-JF7|JL---7\n\
    \7-L-JL7||F7|L7F-7F7|\n\
    \L.L7LFJ|||||FJL7||LJ\n\
    \L7JLJL-JLJLJL--JLJ.L\n"

tests2 :: [(String, Int)]
tests2 = [
    (testInput1, 1), (testInput2, 1), (testInput3, 1), (testInput4, 1),
    (testInput5, 4), (testInput6, 8), (testInput7, 10)]

main :: IO ()
main = do
    s <- readFile "input/10.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
