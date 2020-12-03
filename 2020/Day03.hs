module Main where

import Cartesian
import Utilities
import Data.Set (Set)
import qualified Data.Set as Set

-- Input processing

data Forest = Forest { width :: Int, height :: Int, trees :: Set Position }
    deriving Show
type Input = Forest

parse :: String -> Input
parse s = Forest {
    width = length (head (lines s)),
    height = length (lines s),
    trees = Set.fromList [p | (p, c) <- readGrid s, c == '#']
    }

-- Part One

-- number of elements of ts (repeated to the right indefinitely) on a path
-- going right r and down d at each step
inpath :: Int -> Int -> Forest -> Int
inpath r d forest = Set.size (Set.intersection wrappedPath (trees forest))
  where
    wrappedPath =
       Set.fromList $
           takeWhile (\ (Position _ row) -> row < h) $
           [Position (r*n `mod` w) (d*n) | n <- [1..]]
    w = width forest
    h = height forest

-- right by 3 on each row
solve1 :: Input -> Int
solve1 = inpath 3 1

testInput :: String
testInput = "\
    \..##.......\n\
    \#...#...#..\n\
    \.#....#..#.\n\
    \..#.#...#.#\n\
    \.#...##..#.\n\
    \..#.##.....\n\
    \.#.#.#....#\n\
    \.#........#\n\
    \#.##...#...\n\
    \#...##....#\n\
    \.#..#...#.#\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 7)]

-- Part Two

solve2 :: Input -> Int
solve2 input =
    product [inpath r d input |
        (r, d) <- [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]]

tests2 :: [(String, Int)]
tests2 = [(testInput, 336)]

main :: IO ()
main = do
    s <- readFile "input/03.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
