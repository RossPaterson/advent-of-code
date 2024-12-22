module Main where

import Utilities
import Data.Bits
import Data.List
import qualified Data.Map as Map

-- Input processing

type Input = [Int]

parse :: String -> Input
parse = readNumbers

-- Part One

step :: Int -> Int
step x = step3 (step2 (step1 x))
  where
    step1 n = prune (mix (shiftL n 6) n) -- * 64
    step2 n = prune (mix (shiftR n 5) n) -- `div` 32
    step3 n = prune (mix (shiftL n 11) n) -- * 2024
    mix = xor
    prune n = n .&. 16777215 -- mod 16777216 = 2^24

solve1 :: Input -> Int
solve1 = sum . map (times 2000 step)

testInput :: String
testInput = "\
    \1\n\
    \10\n\
    \100\n\
    \2024\n\
    \"

tests1 :: [(String, Int)]
tests1 = [(testInput, 37327623)]

-- Part Two

-- all the prices generated from a secret
prices :: Int -> [Int]
prices = map price . iterate step
  where
    price n = n `mod` 10

-- differences between succeeding elements
diffs :: [Int] -> [Int]
diffs [] = []
diffs (x:xs) = zipWith (-) xs (x:xs)

-- hashed windows of 4 differences, each paired with the 5th value
diffWindows :: [Int] -> [(Int, Int)]
diffWindows ns =
    zip (zipWith4 hash4 ds (drop 1 ds) (drop 2 ds) (drop 3 ds)) (drop 4 ns)
  where
    hash4 d1 d2 d3 d4 = ((d1*100 + d2)*100 + d3)*100 + d4
    ds = map (+10) (diffs ns)

solve2 :: Input -> Int
solve2 secrets =
    maximum $ Map.unionsWith (+)
        [Map.fromListWith (const id)
            [(w, n) | (w, n) <- diffWindows (take 2001 (prices secret))] |
            secret <- secrets]

testInput2 :: String
testInput2 = "\
    \1\n\
    \2\n\
    \3\n\
    \2024\n\
    \"

tests2 :: [(String, Int)]
tests2 = [(testInput2, 23)]

main :: IO ()
main = do
    s <- readFile "input/22.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
