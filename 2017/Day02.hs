module Main where

import Utilities

type Input = [[Int]]

parse :: String -> Input
parse = map (map read . words) . lines

solve1 :: Input -> Int
solve1 xss = sum [maximum xs - minimum xs | xs <- xss]

tests1 :: [(String, Int)]
tests1 = [("5 1 9 5\n7 5 3\n2 4 6 8\n", 18)]

-- Part Two

check :: [Int] -> Int
check xs = head [x `div` y | x <- xs, y <- xs, x /= y, x `mod` y == 0]

solve2 :: Input -> Int
solve2 xss = sum (map check xss)

tests2 :: [(String, Int)]
tests2 = [("5 9 2 8\n9 4 7 3\n3 8 6 5\n", 9)]

main :: IO ()
main = do
    s <- readFile "input02.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
