module Main where

import Geometry
import Graph
import Utilities
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- Input processing

type Input = Map Position Char

parse :: String -> Input
parse = Map.fromList . readGrid

-- Part One

solve1 :: Input -> Int
solve1 = sum . map price . regions

-- A region is a contiguous set of positions

type Region = Set Position

regions :: Eq a => Map Position a -> [Region]
regions = unfoldr getRegion

getRegion :: Eq a => Map Position a -> Maybe (Region, Map Position a)
getRegion m = do
    (p, c) <- Map.lookupMin m
    let ps = reachable (filter (inRegion m c) . neighbours) p
    return (ps, Map.withoutKeys m ps)

inRegion :: Eq a => Map Position a -> a -> Position -> Bool
inRegion m c p = Map.lookup p m == Just c

neighbours :: Position -> [Position]
neighbours p = [p .+. d | d <- unitVectors]

reachable :: Ord a => (a -> [a]) -> a -> Set a
reachable f x = Set.fromList (concat (bfs f [x]))

-- price calculation for part 1
price :: Region -> Int
price ps = Set.size ps * perimeter ps

perimeter :: Region -> Int
perimeter ps =
    length [(p, p') |
        p <- Set.elems ps, p' <- neighbours p, not (Set.member p' ps)]

testInput1 :: String
testInput1 = "\
    \AAAA\n\
    \BBCD\n\
    \BBCC\n\
    \EEEC\n\
    \"

testInput2 :: String
testInput2 = "\
    \OOOOO\n\
    \OXOXO\n\
    \OOOOO\n\
    \OXOXO\n\
    \OOOOO\n\
    \"

testInput3 :: String
testInput3 = "\
    \RRRRIICCFF\n\
    \RRRRIICCCF\n\
    \VVRRRCCFFF\n\
    \VVRCCCJFFF\n\
    \VVVVCJJCFE\n\
    \VVIVCCJJEE\n\
    \VVIIICJJEE\n\
    \MIIIIIJJEE\n\
    \MIIISIJEEE\n\
    \MMMISSJEEE\n\
    \"

tests1 :: [(String, Int)]
tests1 = [(testInput1, 140), (testInput2, 772), (testInput3, 1930)]

-- Part Two

solve2 :: Input -> Int
solve2 = sum . map price2 . regions

-- price calculation for part 2
price2 :: Region -> Int
price2 ps = Set.size ps * numEdges ps

-- number of edges of a region = number of corners
numEdges :: Region -> Int
numEdges ps =
    length [(p, d) | p <- Set.elems ps, d <- unitVectors, isCorner ps p d]

-- Given a position in the region and a direction (a unit vector), is
-- the corner of the position immediately clockwise from the direction
-- a corner of the region?
isCorner :: Region -> Position -> Position -> Bool
isCorner ps p d =
    -- picture with direction being up:
    -- .?
    -- #.
    not has_d && not has_d' ||
    -- #.
    -- ##
    has_d && has_d' && not (Set.member (p .+. d .+. d') ps)
  where
    d' = rotateSectors 3 d
    has_d = Set.member (p .+. d) ps
    has_d' = Set.member (p .+. d') ps

testInput4 :: String
testInput4 = "\
    \EEEEE\n\
    \EXXXX\n\
    \EEEEE\n\
    \EXXXX\n\
    \EEEEE\n\
    \"

testInput5 :: String
testInput5 = "\
    \AAAAAA\n\
    \AAABBA\n\
    \AAABBA\n\
    \ABBAAA\n\
    \ABBAAA\n\
    \AAAAAA\n\
    \"

tests2 :: [(String, Int)]
tests2 = [
    (testInput1, 80), (testInput2, 436), (testInput4, 236),
    (testInput5, 368), (testInput3, 1206)]

main :: IO ()
main = do
    s <- readFile "input/12.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
