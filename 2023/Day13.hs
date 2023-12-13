module Main where

import Utilities
import Data.List

-- Input processing

type Input = [Pattern]
type Pattern = [[Bool]]

parse :: String -> Input
parse = map pattern . paragraphs
  where
    pattern = map (map (== '#')) . lines

-- Part One

-- vertical or horizontal line
data Reflection = Vertical Int | Horizontal Int
    deriving (Eq, Show)

-- ways of reflecting a pattern
reflections :: Pattern -> [Reflection]
reflections p =
    map Vertical (list_reflections (transpose p)) ++
        map Horizontal (list_reflections p)

-- reflection points of a list
list_reflections :: (Eq a) => [a] -> [Int]
list_reflections xs =
    [length rev_front |
        (rev_front, back) <- zip (rev_inits xs) (tails xs),
        not (null rev_front),
        not (null back),
        and (zipWith (==) rev_front back)]

-- map reverse . inits
rev_inits :: [a] -> [[a]]
rev_inits = scanl (flip (:)) []

-- numerical summary of a reflection
summary :: Reflection -> Int
summary (Vertical n) = n
summary (Horizontal n) = 100*n

solve1 :: Input -> Int
solve1 = sum . map (summary . head . reflections)

testInput :: String
testInput = "\
    \#.##..##.\n\
    \..#.##.#.\n\
    \##......#\n\
    \##......#\n\
    \..#.##.#.\n\
    \..##..##.\n\
    \#.#.##.#.\n\
    \\n\
    \#...##..#\n\
    \#....#..#\n\
    \..##..###\n\
    \#####.##.\n\
    \#####.##.\n\
    \..##..###\n\
    \#....#..#\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 405)]

-- Part Two

-- ways of flipping one bit in the pattern
smudges :: Pattern -> [Pattern]
smudges bss =
    [front_bss ++ (front_bs ++ not b:back_bs):back_bss |
        (front_bss, bs:back_bss) <- splits bss,
        (front_bs, b:back_bs) <- splits bs]

-- extra reflections obtained by smudging
smudge_reflections :: Pattern -> [Reflection]
smudge_reflections p = [r' | p' <- smudges p, r' <- reflections p', r' /= r]
  where
    r = head (reflections p)

solve2 :: Input -> Int
solve2 = sum . map (summary . head . smudge_reflections)

tests2 :: [(String, Int)]
tests2 = [(testInput, 400)]

main :: IO ()
main = do
    s <- readFile "input/13.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
