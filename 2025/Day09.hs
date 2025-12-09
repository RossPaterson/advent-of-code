module Main where

import Geometry
import Parser
import Utilities
import Data.List (sortOn)
import Data.Maybe (isNothing)
import Data.Ord (Down(Down))

-- Input processing

type Input = [Position]

parse :: String -> Input
parse = map (runParser position) . lines
  where
    position = Position <$> nat <* char ',' <*> nat

-- Part One

solve1 :: Input -> Int
solve1 = maximum . map boxSize . rectangles

-- all possible rectangles defined by a collection of points
rectangles :: [Position] -> [AABox Position]
rectangles ps = [rectangle p1 p2 | p1 <- ps, p2 <- ps]

-- axis-aligned rectangle with the given corners
rectangle :: Position -> Position -> AABox Position
rectangle p1 p2 = boundingBox [p1, p2]

testInput :: String
testInput = "\
    \7,1\n\
    \11,1\n\
    \11,7\n\
    \9,7\n\
    \9,5\n\
    \2,5\n\
    \2,3\n\
    \7,3\n\
    \"

tests1 :: [(String, Int)]
tests1 = [(testInput, 50)]

-- Part Two

-- size of the largest rectangle that doesn't have an edge going into or
-- across it
-- This is equivalent to being within the path, provided there are no
-- parallel edges immediately adjacent to each other, which holds for
-- our inputs.
solve2 :: Input -> Int
solve2 ps =
    boxSize $ head $ filter whole $ sortOn (Down . boxSize) $ rectangles ps
  where
    -- longer edges are more likely to eliminate boxes
    es = sortOn (Down . boxSize) $ edges ps

    -- rectangle doesn't have an edge going into or across it
    whole rect = and [isNothing (intersectBox interior e) | e <- es]
      where
        interior = growBox (-1) rect

-- General optimization (useful if p is expensive):
--
-- maximum . map f . filter p = f . head . filter p . sortOn (Down . f)

-- a thin bounding box for each of the edges of the path, which are
-- specified as being either horizontal or vertical
edges :: [Position] -> [AABox Position]
edges [] = []
edges (p:ps) = zipWith rectangle (p:ps) (ps++[p])

tests2 :: [(String, Int)]
tests2 = [(testInput, 24)]

main :: IO ()
main = do
    s <- readFile "input/09.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
