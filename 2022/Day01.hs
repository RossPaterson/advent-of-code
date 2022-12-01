module Main where

import Utilities
import Data.List

-- Input processing

type Input = [[Int]]

parse :: String -> Input
parse = map readNumbers . paragraphs

-- Part One

solve1 :: Input -> Int
solve1 = maximum . map sum

testInput :: String
testInput = "\
    \1000\n\
    \2000\n\
    \3000\n\
    \\n\
    \4000\n\
    \\n\
    \5000\n\
    \6000\n\
    \\n\
    \7000\n\
    \8000\n\
    \9000\n\
    \\n\
    \10000\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 24000)]

-- Part Two

solve2 :: Input -> Int
solve2 = sum . take 3 . reverse . sort . map sum

tests2 :: [(String, Int)]
tests2 = [(testInput, 45000)]

main :: IO ()
main = do
    s <- readFile "input/01.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
