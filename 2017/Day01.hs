module Main where

import Utilities
import Data.Char

type Input = [Int]

parse :: String -> Input
parse = map digitToInt . filter isDigit

rotl :: [a] -> [a]
rotl [] = []
rotl (x:xs) = xs ++ [x]

solve1 :: Input -> Int
solve1 xs = sum [x | (x, y) <- zip xs (rotl xs), x == y]

tests1 :: [(String, Int)]
tests1 = [("1122", 3), ("1111", 4), ("1234", 0), ("91212129", 9)]

-- Part Two

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

solve2 :: Input -> Int
solve2 xs = sum [x | (x, y) <- zip xs (rotate (length xs `div` 2) xs), x == y]

tests2 :: [(String, Int)]
tests2 = [("1212", 6), ("1221", 0), ("123425", 4), ("123123", 12), ("12131415", 4)]

main :: IO ()
main = do
    s <- readFile "input01.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
