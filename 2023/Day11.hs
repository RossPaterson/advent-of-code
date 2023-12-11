module Main where

import Geometry
import Utilities
import Data.List
import Data.Map (Map, (!))
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Input processing

type Input = [Position]

parse :: String -> Input
parse s = [p | (p, c) <- readGrid s, c == '#']

-- Part One

-- replace each empty row or column by factor rows or columns
expand :: Int -> [Position] -> [Position]
expand factor ps = [Position (x_map!x) (y_map!y) | Position x y <- ps]
  where
    x_map = expansion_map factor [x | Position x _ <- ps]
    y_map = expansion_map factor [y | Position _ y <- ps]

-- map of numbers to their values after expansion
expansion_map :: Int -> [Int] -> Map Int Int
expansion_map factor all_ns =
    Map.fromList (zip ns (expand_list factor ns))
  where
    ns = Set.elems (Set.fromList all_ns)

-- expand the gaps in the list by factor
expand_list :: Int -> [Int] -> [Int]
expand_list _ [] = []
expand_list factor (n:ns) =
    scanl (+) n [factor*(d-1)+1 | d <- diffs (n:ns)]

-- differences between successive numbers
diffs :: [Int] -> [Int]
diffs [] = []
diffs (n:ns) = zipWith (-) ns (n:ns)

-- shortest paths between each pair of positions
shortestPaths :: [Position] -> [Int]
shortestPaths ps =
    [distance p1 p2 | p1:ps' <- tails ps, p2 <- ps']

-- sum of paths after expanding by factor
path_sum :: Int -> [Position] -> Int
path_sum factor = sum . shortestPaths . expand factor

solve1 :: Input -> Int
solve1 = path_sum 2

testInput :: String
testInput = "\
    \...#......\n\
    \.......#..\n\
    \#.........\n\
    \..........\n\
    \......#...\n\
    \.#........\n\
    \.........#\n\
    \..........\n\
    \.......#..\n\
    \#...#.....\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 374)]

-- Part Two

solve2 :: Input -> Int
solve2 = path_sum 1000000

tests2 :: [((Int, String), Int)]
tests2 = [((10, testInput), 1030), ((100, testInput), 8410)]

main :: IO ()
main = do
    s <- readFile "input/11.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (uncurry path_sum . fmap parse) tests2))
    print (solve2 input)
