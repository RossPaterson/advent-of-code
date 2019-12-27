module Main where

import Utilities

solve1 :: String -> Int
solve1 = sum . map move

move :: Char -> Int
move '(' = 1
move ')' = -1
move _ = 0

tests1 :: [(String, Int)]
tests1 = [
    ("(())", 0),
    ("()()", 0),
    ("(((", 3),
    ("(()(()(", 3),
    ("))(((((", 3),
    ("())", -1),
    ("))(", -1),
    (")))", -3),
    (")())())", -3)]

-- Part Two --

solve2 :: String -> Int
solve2 = length . takeWhile (/= -1) . scanl (+) 0 . map move

tests2 :: [(String, Int)]
tests2 = [
    (")", 1),
    ("()())", 5)]

main :: IO ()
main = do
    s <- readFile "input/01.txt"
    putStr (unlines (failures "solve1" solve1 tests1))
    print (solve1 s)
    putStr (unlines (failures "solve2" solve2 tests2))
    print (solve2 s)
