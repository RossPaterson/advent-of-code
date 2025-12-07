module Main where

import Geometry
import Utilities
import Data.List (mapAccumL)
import Data.Bag (Bag)
import qualified Data.Bag as Bag
import Data.Set (Set)
import qualified Data.Set as Set

-- Input processing

type Input = (Position, Set Position, Int)

parse :: String -> Input
parse s = (start, splitters, ymax)
  where
    pcs = readGrid s
    start = head [p | (p, c) <- pcs, c == 'S']
    splitters = Set.fromList [p | (p, c) <- pcs, c == '^']
    ymax = maximum [y | (Position _ y, _) <- pcs]

-- Part One

solve1 :: Input -> Int
solve1 (Position x0 y0, splitters, ymax) =
    sum $ snd $ mapAccumL (splitBeams splitters) (Set.singleton x0) [y0..ymax]

-- Split the beams with the splitters on row y, and also return the number
-- of input beams that were split.
splitBeams :: Set Position -> Set Int -> Int -> (Set Int, Int)
splitBeams splitters xs y = (new_xs, split_count)
  where
    new_xs = Set.fromList (concat split_xs)
    split_xs = map (splitBeam splitters y) (Set.elems xs)
    split_count = length [n | n <- split_xs, length n == 2]

-- Split the beam at position x in two if at a splitter on row y,
-- and otherwise leave it alone.
splitBeam :: Set Position -> Int -> Int -> [Int]
splitBeam splitters y x
  | Set.member (Position x y) splitters = [x-1, x+1]
  | otherwise = [x]

testInput :: String
testInput = "\
    \.......S.......\n\
    \...............\n\
    \.......^.......\n\
    \...............\n\
    \......^.^......\n\
    \...............\n\
    \.....^.^.^.....\n\
    \...............\n\
    \....^.^...^....\n\
    \...............\n\
    \...^.^...^.^...\n\
    \...............\n\
    \..^...^.....^..\n\
    \...............\n\
    \.^.^.^.^.^...^.\n\
    \...............\n\
    \"

tests1 :: [(String, Int)]
tests1 = [(testInput, 21)]

-- Part Two

-- Recurring idea: if the order does not matter, large groups of
-- indistinguishable elements can be replaced with a count for each value,
-- i.e. use a bag instead of a list.

solve2 :: Input -> Int
solve2 (Position x0 y0, splitters, ymax) =
    Bag.size $ foldl (splitMultiBeams splitters) (Bag.singleton x0) [y0..ymax]

-- Like splitBeams, except with bags of beams rather than sets
-- (and we don't need to count how many were split).
splitMultiBeams :: Set Position -> Bag Int -> Int -> Bag Int
splitMultiBeams splitters xs y =
    Bag.fromCounts [(x', n) |
        (x, n) <- Bag.counts xs, x' <- splitBeam splitters y x]

tests2 :: [(String, Int)]
tests2 = [(testInput, 40)]

main :: IO ()
main = do
    s <- readFile "input/07.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
