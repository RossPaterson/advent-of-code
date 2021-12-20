module Main where

import Utilities
import Geometry
import Data.Set (Set)
import qualified Data.Set as Set

-- Input processing

type Algorithm = Set Int
type Pixels = Set Position

type Input = (Algorithm, Pixels)

parse :: String -> Input
parse s = (algo, pixels)
  where
    [p1, p2] = paragraphs s
    algo = Set.fromList
        [n | (n, c) <- zip [0..] (filter (/= '\n') p1), c == '#']
    pixels = Set.fromList [p | (p, c) <- readGrid p2, c == '#']

-- Part One

-- locations within n steps in all directions, in row-major order
neighbours :: Int -> Position -> [Position]
neighbours n (Position x y) =
    [Position (x+dx) (y+dy) | dy <- [-n..n], dx <- [-n..n]]

-- locations within n steps in all directions of points in g
halo :: Int -> Pixels -> [Position]
halo n g = fastNub [p' | p <- Set.elems g, p' <- neighbours n p]

bitsToInt :: [Bool] -> Int
bitsToInt = foldl add 0 . map fromEnum
  where
    add n b = n*2 + b

-- single enhancement step for a single pixel
enhance_pixel :: Algorithm -> Pixels -> Position -> Bool
enhance_pixel alg g p =
    Set.member (bitsToInt [Set.member p' g | p' <- neighbours 1 p]) alg

-- A single enhancement step, assuming that an all-empty neighbourhood
-- becomes 1 (true of the test input, but not the actual input).
enhance :: Algorithm -> Pixels -> Pixels
enhance alg g = Set.fromList [p | p <- halo 1 g, enhance_pixel alg g p]

-- Two enhance steps, assuming that an all-empty neighbourhood will
-- become 0 after one or two steps (true of both the test input and the
-- actual input).
enhance2 :: Algorithm -> Pixels -> Pixels
enhance2 alg g = Set.fromList [p | p <- halo 2 g, enhance_pixel alg g' p]
  where
    g' = Set.fromList [p | p <- halo 3 g, enhance_pixel alg g p]

-- 2 enhance steps
solve1 :: Input -> Int
solve1 (alg, g) = Set.size (enhance2 alg g)

testInput :: String
testInput = "\
    \..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..##\
    \#..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###\
    \.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#.\
    \.#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#.....\
    \.#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#..\
    \...####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.....\
    \..##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#\n\
    \\n\
    \#..#.\n\
    \#....\n\
    \##..#\n\
    \..#..\n\
    \..###\n\
    \"

tests1 :: [(String, Int)]
tests1 = [(testInput, 35)]

-- Part Two

-- 50 enhance steps
solve2 :: Input -> Int
solve2 (alg, g) = Set.size (times 25 (enhance2 alg) g)

tests2 :: [(String, Int)]
tests2 = [(testInput, 3351)]

main :: IO ()
main = do
    s <- readFile "input/20.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
