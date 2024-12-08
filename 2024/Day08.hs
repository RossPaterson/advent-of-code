module Main where

import Geometry
import Utilities
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- Input processing

type Input = (AABox Position, Map Frequency [Position])

type Frequency = Char

parse :: String -> Input
parse s = (box, antennas)
  where
    pcs = readGrid s
    box = boundingBox (map fst pcs)
    antennas = Map.fromListWith (++) [(c, [p]) | (p, c) <- pcs, c /= '.']

-- Part One

solve1 :: Input -> Int
solve1 (box, m) =
    Set.size $ Set.filter (flip inBox box) $ allAntinodes m

allAntinodes :: Map Frequency [Position] -> Set Position
allAntinodes = Set.unions . map antinodes . Map.elems

antinodes :: [Position] -> Set Position
antinodes ps =
    Set.fromList [antinode p1 p2 | (p1, p2) <- allPairs ps]

-- same distance beyond p2
antinode :: Position -> Position -> Position
antinode p1 p2 = p2 .+. (p2 .-. p1)

-- all pairs of distinct elements in the list, in either order
allPairs :: [a] -> [(a, a)]
allPairs xs =
    [p | x1:rest <- tails xs, x2 <- rest, p <- [(x1, x2), (x2, x1)]]

testInput :: String
testInput = "\
    \............\n\
    \........0...\n\
    \.....0......\n\
    \.......0....\n\
    \....0.......\n\
    \......A.....\n\
    \............\n\
    \............\n\
    \........A...\n\
    \.........A..\n\
    \............\n\
    \............\n\
    \"

tests1 :: [(String, Int)]
tests1 = [(testInput, 14)]

-- Part Two

solve2 :: Input -> Int
solve2 (box, m) =
    Set.size $ allAntinodes2 box m

allAntinodes2 :: AABox Position -> Map Frequency [Position] -> Set Position
allAntinodes2 box m =
    Set.unions [antinodes2 box ps | ps <- Map.elems m]

antinodes2 :: AABox Position -> [Position] -> Set Position
antinodes2 box ps =
    Set.unions [resonant box p1 p2 | (p1, p2) <- allPairs ps]

resonant :: AABox Position -> Position -> Position -> Set Position
resonant box p1 p2 =
    Set.fromList $
        takeWhile (flip inBox box) (iterate (.+. (p2 .-. p1)) p2)

tests2 :: [(String, Int)]
tests2 = [(testInput, 34)]

main :: IO ()
main = do
    s <- readFile "input/08.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
