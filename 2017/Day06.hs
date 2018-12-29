module Main where

import Utilities
import Data.List
import qualified Data.Map as Map
import Data.Map (Map, (!))

type Input = [Int]

parse :: String -> Input
parse = map read . words

redistribute :: Input -> Input
redistribute xs = zipWith (+) (front ++ 0:back) (incr_front ++ incr_back)
  where
    -- first first occurrence of largest element
    (front, v:back) = span (/= maximum xs) xs
    -- distribute its values evenly, starting at the following element
    n = length xs
    (d, r) = v `divMod` n
    increments = replicate r (d+1) ++ replicate (n-r) d
    (incr_back, incr_front) = splitAt (n - length front - 1) increments

-- positions of the first and second occurrences of the first repeated element
findRepetition :: Ord a => [a] -> (Int, Int)
findRepetition = iter Map.empty 0
  where
    iter s n (x:xs)
      | Map.member x s = (s!x, Map.size s)
      | otherwise = iter (Map.insert x n s) (n+1) xs

solve1 :: Input -> Int
solve1 xs = p2
  where
    (p1, p2) = findRepetition (iterate redistribute xs)

tests1 :: [(String, Int)]
tests1 = [("0 2 7 0", 5)]

-- Part Two

solve2 :: Input -> Int
solve2 xs = p2 - p1
  where
    (p1, p2) = findRepetition (iterate redistribute xs)

tests2 :: [(String, Int)]
tests2 = [("0 2 7 0", 4)]

main :: IO ()
main = do
    s <- readFile "input/06.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
