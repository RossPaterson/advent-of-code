module Main where

import Geometry
import Utilities
import Data.Char
import Data.List

-- Input processing

type Input = Schematic
data Schematic = Schematic {
    symbols :: [(Position, Char)],     -- non-digit, non-period chars
    numbers :: [(AABox Position, Int)] -- area of each number in the grid
    }
    deriving (Show)

parse :: String -> Input
parse s = Schematic {
    symbols = [(p, c) | (p, c) <- readGrid s, c /= '.', not (isDigit c)],
    numbers = [(box, n) |
        (y, l) <- zip [0..] (lines s),
        xcs <- genWords (isDigit . snd) (zip [0..] l),
        let x = fst (head xcs),
        let n = read (map snd xcs),
        let sz = length xcs,
        let box = boundingBox [Position x y, Position (x+sz-1) y]]
    }

-- maximax non-empty subsequences of elements satisfying p.
-- words = genWords (not . isSpace)
genWords :: (a -> Bool) -> [a] -> [[a]]
genWords p = unfoldr getWord
  where
    getWord xs = case dropWhile (not . p) xs  of
        [] -> Nothing
        xs' -> Just (span p xs')

-- Part One

-- a part is a number adjacent to a symbol
parts :: Schematic -> [Int]
parts s =
    [n |
        (box, n) <- numbers s,
        let boxPlus1 = growBox 1 box,
        or [inBox sp boxPlus1 | sp <- map fst (symbols s)]]

solve1 :: Input -> Int
solve1 = sum . parts

testInput :: String
testInput = "\
    \467..114..\n\
    \...*......\n\
    \..35..633.\n\
    \......#...\n\
    \617*......\n\
    \.....+.58.\n\
    \..592.....\n\
    \......755.\n\
    \...$.*....\n\
    \.664.598..\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 4361)]

-- Part Two

-- a gear is an asterisk adjacent to exactly two numbers
gears :: Schematic -> [(Int, Int)]
gears s =
    [(head ns, head (tail ns)) |
        star_pos <- [p | (p, c) <- symbols s, c == '*'],
        let ns = [n | (box, n) <- numbers s, inBox star_pos (growBox 1 box)],
        length ns == 2]

solve2 :: Input -> Int
solve2 = sum . map (uncurry (*)) . gears

tests2 :: [(String, Int)]
tests2 = [(testInput, 467835)]

main :: IO ()
main = do
    s <- readFile "input/03.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
