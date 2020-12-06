module Main where

import Utilities
import Data.Set (Set)
import qualified Data.Set as Set

-- Input processing

type Input = [[Set Char]]

parse :: String -> Input
parse = map (map Set.fromList . lines) . paragraphs

-- Part One

-- number of questions to which anyone in a group answered "yes"
solve1 :: Input -> Int
solve1 = sum . map (Set.size . Set.unions)

testInput :: String
testInput = "\
    \abc\n\
    \\n\
    \a\n\
    \b\n\
    \c\n\
    \\n\
    \ab\n\
    \ac\n\
    \\n\
    \a\n\
    \a\n\
    \a\n\
    \a\n\
    \\n\
    \b\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 11)]

-- Part Two

-- number of questions to which everyone in a group answered "yes"
solve2 :: Input -> Int
solve2 = sum . map (Set.size . foldr1 Set.intersection)

tests2 :: [(String, Int)]
tests2 = [(testInput, 6)]

main :: IO ()
main = do
    s <- readFile "input/06.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
