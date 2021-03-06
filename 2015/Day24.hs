module Main where

import Utilities

type Input = [Int]

parse :: String -> Input
parse = map read . lines

-- partitions of xs for which the first list sums to n
selections :: Int -> [Int] -> [([Int], [Int])]
selections n [] = [([], []) | n == 0]
selections n (x:xs) =
    [(x:sel, rest) | x <= n, (sel, rest) <- selections (n-x) xs] ++
    [(sel, x:rest) | (sel, rest) <- selections n xs]

firstGroups :: Int -> [Int] -> [[Int]]
firstGroups ngroups xs
  | total `mod` ngroups /= 0 = []
  | otherwise = [sel | (sel, rest) <- selections n xs, evenSplit (ngroups-1) n rest]
  where
    total = sum xs
    n = total `div` ngroups

evenSplit :: Int -> Int -> [Int] -> Bool
evenSplit 1 _ _ = True
evenSplit ngroups n xs = any (evenSplit (ngroups-1) n) (drops n xs)

-- drops n = map snd . selections n
drops :: Int -> [Int] -> [[Int]]
drops n [] = [[] | n == 0]
drops n (x:xs) =
    [rest | x <= n, rest <- drops (n-x) xs] ++
    [x:rest | rest <- drops n xs]

solve1 :: Input -> Int
solve1 = minimum . map product . leastBy length . firstGroups 3

test :: [Int]
test = [1..5] ++ [7..11]

-- Part Two --

firstGroups2 :: [Int] -> [[Int]]
firstGroups2 xs
  | total `mod` 4 /= 0 = []
  | otherwise = [sel | (sel, rest) <- selections n xs, not (null (selections n rest))]
  where
    total = sum xs
    n = total `div` 3

solve2 :: Input -> Int
solve2 = minimum . map product . leastBy length . firstGroups 4

main :: IO ()
main = do
    s <- readFile "input/24.txt"
    let input = parse s
    print (solve1 input)
    print (solve2 input)
