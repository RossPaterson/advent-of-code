module Main where

import Utilities
import Data.List
import qualified Data.Map as Map

-- Input processing

-- the input is described as two lists of equal length
type Input = ([Int], [Int])

parse :: String -> Input
parse = unzip . map getPair . lines
  where
    getPair s = case map read (words s) of
        [n1, n2] -> (n1, n2)
        _ -> error "parse: not a pair"

-- Part One

solve1 :: Input -> Int
solve1 (xs, ys) =
    sum $ map abs $ zipWith (-) (sort xs) (sort ys)

testInput :: String
testInput = "\
    \3   4\n\
    \4   3\n\
    \2   5\n\
    \1   3\n\
    \3   9\n\
    \3   3\n\
    \"

tests1 :: [(String, Int)]
tests1 = [(testInput, 11)]

-- Part Two

solve2 :: Input -> Int
solve2 (xs, ys) =
    sum $ map (uncurry (*)) $ Map.assocs $
        Map.intersectionWith (*) (frequencyMap xs) (frequencyMap ys)

tests2 :: [(String, Int)]
tests2 = [(testInput, 31)]

main :: IO ()
main = do
    s <- readFile "input/01.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
