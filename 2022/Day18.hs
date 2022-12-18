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

neighbours :: Point3 -> [Point3]
neighbours p = [p .+. d | d <- unitVectors]

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

data Box = Box Point3 Point3
    deriving (Show)

-- smallest box with clear space around the set on all sides
surroundingBox :: Set Point3 -> Box
surroundingBox ps =
    Box (Point3 (min_x-1) (min_y-1) (min_z-1))
        (Point3 (max_x+1) (max_y+1) (max_z+1))
  where
    min_x = minimum [x | Point3 x _ _ <- Set.elems ps]
    min_y = minimum [y | Point3 _ y _ <- Set.elems ps]
    min_z = minimum [z | Point3 _ _ z <- Set.elems ps]
    max_x = maximum [x | Point3 x _ _ <- Set.elems ps]
    max_y = maximum [y | Point3 _ y _ <- Set.elems ps]
    max_z = maximum [z | Point3 _ _ z <- Set.elems ps]

contents :: Box -> Set Point3
contents (Box (Point3 min_x min_y min_z) (Point3 max_x max_y max_z)) =
    Set.fromList [Point3 x y z |
        x <- [min_x..max_x], y <- [min_y..max_y], z <- [min_z..max_z]]

inside :: Point3 -> Box -> Bool
inside (Point3 x y z)
        (Box (Point3 min_x min_y min_z) (Point3 max_x max_y max_z)) =
    min_x <= x && x <= max_x &&
    min_y <= y && y <= max_y &&
    min_z <= z && z <= max_z

-- all points not reachable from outside the set
interior :: Set Point3 -> Set Point3
interior ps =
    Set.difference (contents box) $
        Set.fromList [p | level <- bfs neighbours_in_box [corner], p <- level]
  where
    neighbours_in_box p =
        [p' | p' <- neighbours p, inside p' box, not (Set.member p' ps)]
    box@(Box corner _) = surroundingBox ps

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
