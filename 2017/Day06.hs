module Main where

import Utilities
import qualified RationalList as RL

type Input = Banks
type Banks = [Int]

parse :: String -> Input
parse = map read . words

redistribute :: Banks -> Banks
redistribute xs = zipWith (+) (front ++ 0:back) (incr_front ++ incr_back)
  where
    -- first occurrence of largest element
    (front, v:back) = span (/= maximum xs) xs
    -- distribute its values evenly, starting at the following element
    n = length xs
    (d, r) = v `divMod` n
    increments = replicate r (d+1) ++ replicate (n-r) d
    (incr_back, incr_front) = splitAt (n - length front - 1) increments

solve1 :: Input -> Int
solve1 xs = length (RL.prefix rl) + length (RL.repetend rl)
  where
    rl = RL.iterate redistribute xs

testInput :: String
testInput = "0 2 7 0"

tests1 :: [(String, Int)]
tests1 = [(testInput, 5)]

-- Part Two

solve2 :: Input -> Int
solve2 = length . RL.repetend . RL.iterate redistribute

tests2 :: [(String, Int)]
tests2 = [(testInput, 4)]

main :: IO ()
main = do
    s <- readFile "input/06.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
