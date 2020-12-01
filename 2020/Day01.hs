module Main where

import Utilities

-- Input processing

type Input = [Int]

parse :: String -> Input
parse = map read . lines

-- Part One

solve1 :: Input -> Int
solve1 xs = head [product ns | (ns, _) <- choose 2 xs, sum ns == 2020]

tests1 :: [(String, Int)]
tests1 = [(testInput, 514579)]

testInput :: String
testInput = "\
    \1721\n\
    \979\n\
    \366\n\
    \299\n\
    \675\n\
    \1456\n"

-- Part Two

solve2 :: Input -> Int
solve2 xs = head [product ns | (ns, _) <- choose 3 xs, sum ns == 2020]

tests2 :: [(String, Int)]
tests2 = [(testInput, 241861950)]

main :: IO ()
main = do
    s <- readFile "input/01.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
