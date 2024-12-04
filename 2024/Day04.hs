module Main where

import Geometry
import Utilities
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

-- Input processing

type Input = Map Position Char

parse :: String -> Input
parse = Map.fromList . readGrid

-- Part One

solve1 :: Input -> Int
solve1 = countWord "XMAS"

-- number of ways of placing the list in the grid in any direction
countWord :: Eq a => [a] -> Map Position a -> Int
countWord w m =
    length [(p, dp) |
        p <- Map.keys m,
        dp <- unitVectors ++ corners,
        just_w `isPrefixOf` [Map.lookup p' m | p' <- iterate (.+. dp) p]]
  where
    just_w = map Just w

testInput1 :: String
testInput1 = "\
    \..X...\n\
    \.SAMX.\n\
    \.A..A.\n\
    \XMAS.S\n\
    \.X....\n\
    \"

testInput2 :: String
testInput2 = "\
    \MMMSXXMASM\n\
    \MSAMXMSMSA\n\
    \AMXSXMAAMM\n\
    \MSAMASMSMX\n\
    \XMASAMXAMM\n\
    \XXAMMXXAMA\n\
    \SMSMSASXSS\n\
    \SAXAMASAAA\n\
    \MAMMMXMMMM\n\
    \MXMXAXMASX\n\
    \"

tests1 :: [(String, Int)]
tests1 = [(testInput1, 4), (testInput2, 18)]

-- Part Two

solve2 :: Input -> Int
solve2 m =
    length [p |
        (p, c) <- Map.assocs m, c == 'A',
        mas m p (Position 1 1), mas m p (Position (-1) 1)]

mas :: Map Position Char -> Position -> Position -> Bool
mas m p dp =
    sort [Map.lookup (p .+. dp) m, Map.lookup (p .-. dp) m] ==
        [Just 'M', Just 'S']

tests2 :: [(String, Int)]
tests2 = [(testInput2, 9)]

main :: IO ()
main = do
    s <- readFile "input/04.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
