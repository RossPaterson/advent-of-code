module Main where

import Utilities
import Geometry
import Graph
import Data.Char
import Data.List
import Data.Map (Map, (!))
import qualified Data.Map as Map

-- Input processing

type Input = Map Position Int

parse :: String -> Input
parse = Map.fromList . map (fmap digitToInt) . readGrid

-- Part One

-- neighbouring positions of a location on the map
neighbours :: Map Position a -> Position -> [Position]
neighbours m p = [p' | d <- unitVectors, let p' = p .+. d, Map.member p' m]

-- local minima in the map
lowPoints :: Ord a => Map Position a -> [(Position, a)]
lowPoints m =
    [(p, v) | (p, v) <- Map.assocs m, and [v < m!p' | p' <- neighbours m p]]

solve1 :: Input -> Int
solve1 m = sum [v+1 | (_, v) <- lowPoints m]

testInput :: String
testInput = "\
    \2199943210\n\
    \3987894921\n\
    \9856789892\n\
    \8767896789\n\
    \9899965678\n\
    \"

tests1 :: [(String, Int)]
tests1 = [(testInput, 15)]

-- Part Two

-- All the basins in a heightmap.
-- Locations of height 9 do not belong to any basin.  We are told that
-- every other location belongs to the basin of exactly one lowpoint.
basins :: Map Position Int -> [[Position]]
basins m = [fill m' p | (p, _) <- lowPoints m]
  where
    m' = Map.filter (/= 9) m

-- all points in m reachable from p
fill :: Map Position a -> Position -> [Position]
fill m p = concat (bfs (neighbours m) [p])

solve2 :: Input -> Int
solve2 = product . take 3 . reverse . sort . map length . basins

tests2 :: [(String, Int)]
tests2 = [(testInput, 1134)]

main :: IO ()
main = do
    s <- readFile "input/09.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
