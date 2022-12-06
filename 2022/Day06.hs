module Main where

import Utilities
import Data.List

-- Input processing

type Input = String

parse :: String -> Input
parse s = head (lines s)

-- Part One

allDifferent :: Eq a => [a] -> Bool
allDifferent [] = True
allDifferent (x:xs) = not (elem x xs) && allDifferent xs

marker :: Eq a => Int -> [a] -> Bool
marker w xs = length xs >= w && allDifferent (take w (reverse xs))

-- naive version, which is fast enough for both parts
firstMarker :: Eq a => Int -> [a] -> Int
firstMarker w = length . takeWhile (not . marker w) . inits

-- faster version, but still quadratic in w
firstMarker' :: Eq a => Int -> [a] -> Int
firstMarker' w xs =
    w + length (takeWhile (not . allDifferent) (drop w (windows w xs)))

-- reversed windows of length up to w
windows :: Int -> [a] -> [[a]]
windows w xs = map (take w) (rev_inits xs)

-- map reverse . inits
rev_inits :: [a] -> [[a]]
rev_inits = scanl (flip (:)) []

-- linear in w (but similar time to previous on this problem size)
firstMarker'' :: Eq a => Int -> [a] -> Int
firstMarker'' w xs =
    w + length (takeWhile (not . all_big) (drop w (windows w diff_counts)))
  where
    -- Does each element xk in the window have at least w-k followers
    -- that are different from it?
    all_big diffs = all (>= w) (zipWith (+) [1..] diffs)
    -- for each x, min of w-1 and number of consecutive different elements
    diff_counts = map diff_followers (tail (windows w xs))
    diff_followers [] = error "diff_followers []"
    diff_followers (y:ys) = length (takeWhile (/= y) ys)

solve1 :: Input -> Int
solve1 = firstMarker'' 4

tests1 :: [(String, Int)]
tests1 = [
    ("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 7),
    ("bvwbjplbgvbhsrlpgdmjqwftvncz", 5),
    ("nppdvjthqldpwncqszvftbrmjlhg", 6),
    ("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 10),
    ("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 11)]

-- Part Two

solve2 :: Input -> Int
solve2 = firstMarker'' 14

tests2 :: [(String, Int)]
tests2 = [
    ("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 19),
    ("bvwbjplbgvbhsrlpgdmjqwftvncz", 23),
    ("nppdvjthqldpwncqszvftbrmjlhg", 23),
    ("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 29),
    ("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 26)]

main :: IO ()
main = do
    s <- readFile "input/06.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
