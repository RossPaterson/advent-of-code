module Main where

import Utilities
import Geometry
import Graph
import Parser
import Data.Set (Set)
import qualified Data.Set as Set

-- Input processing

type Input = Set Point3

parse :: String -> Input
parse = Set.fromList . map (runParser point) . lines
  where
    point = Point3 <$> nat <* char ',' <*> nat <* char ',' <*> nat

-- Part One

-- The six neighbouring points of a point
neighbours :: Point3 -> [Point3]
neighbours p = [p .+. d | d <- unitVectors]

-- Total number of cube sides between points in and not in the set
surfaceArea :: Set Point3 -> Int
surfaceArea ps =
    sum [length [p' | p' <- neighbours p, not (Set.member p' ps)] |
        p <- Set.elems ps]

solve1 :: Input -> Int
solve1 = surfaceArea

testInput :: String
testInput = "\
    \2,2,2\n\
    \1,2,2\n\
    \3,2,2\n\
    \2,1,2\n\
    \2,3,2\n\
    \2,2,1\n\
    \2,2,3\n\
    \2,2,4\n\
    \2,2,6\n\
    \1,2,5\n\
    \3,2,5\n\
    \2,1,5\n\
    \2,3,5\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 64)]

-- Part Two

-- All points not reachable from outside the set
interior :: Set Point3 -> Set Point3
interior ps = Set.difference (Set.fromList (boxElements box)) exterior
  where
    -- smallest box with clear space around the set on all sides
    box = growBox 1 (boundingBox ps)
    -- points in the box reachable from outside the set
    exterior =
        Set.fromList [p | level <- bfs neighbours_in_box start, p <- level]
    start = [minCorner box, maxCorner box]
    neighbours_in_box p =
        [p' | p' <- neighbours p, inBox p' box, not (Set.member p' ps)]

solve2 :: Input -> Int
solve2 = surfaceArea . interior

tests2 :: [(String, Int)]
tests2 = [(testInput, 58)]

main :: IO ()
main = do
    s <- readFile "input/18.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
