module Main where

import Utilities
import Data.Char
import Data.List

-- Input processing

type Input = Grid Int
type Grid a = [[a]]

parse :: String -> Input
parse s = map (map digitToInt) (lines s)

-- Part One

-- Is each tree visible from the left of the row?
-- That is, is the value larger than all preceding ones?
visibleLeft :: [Int] -> [Bool]
visibleLeft hs = zipWith (>) hs (scanl max (-1) hs)

-- Apply a length-preserving row transformation in all 4 ways: left-right,
-- right-left, up-down and down-up, combining the results at each point
-- in the grid with an associative commutative operator.
allWays :: ([a] -> [b]) -> (b -> b -> b) -> Grid a -> Grid b
allWays row op xss =
    zipWith (zipWith op)
        (map bothWays xss)
        (transpose (map bothWays (transpose xss)))
  where
    bothWays xs = zipWith op (row xs) (reverse (row (reverse xs)))

-- Reduce the values in a grid with an associative commutative operator.
foldGrid :: (a -> a -> a) -> Grid a -> a
foldGrid op xss = foldr1 op (map (foldr1 op) xss)

solve1 :: Input -> Int
solve1 = foldGrid (+) . map (map fromEnum) . allWays visibleLeft (||)

testInput :: String
testInput = "\
    \30373\n\
    \25512\n\
    \65332\n\
    \33549\n\
    \35390\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 21)]

-- Part Two

-- The number of trees visible to the right of each tree
viewRight :: Ord a => [a] -> [Int]
viewRight = map view . init . tails
  where
    view [] = error "view []"
    -- view (h:hs) = min (length hs) (length (takeWhile (<h) hs) + 1)
    view [_] = 0
    view (h:hs) = length (takeWhile (<h) (init hs)) + 1

solve2 :: Input -> Int
solve2 = foldGrid (max) . allWays viewRight (*)

tests2 :: [(String, Int)]
tests2 = [(testInput, 8)]

main :: IO ()
main = do
    s <- readFile "input/08.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
