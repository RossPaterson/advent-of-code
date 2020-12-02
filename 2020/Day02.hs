module Main where

import Utilities
import Parser
import Control.Applicative
import Data.Char

-- Input processing

data Policy = Policy Int Int Char String
    deriving Show

type Input = [Policy]

parse :: String -> Input
parse = map (runParser policy) . lines
  where
    policy = Policy <$> nat <* char '-' <*> nat <* char ' ' <*>
        satisfy isLower <* string ": " <*> some (satisfy isAlpha)

-- Part One

-- number of occurrences of x in the list
occurrences :: Eq a => a -> [a] -> Int
occurrences x = length . filter (== x)

valid1 :: Policy -> Bool
valid1 (Policy low high c pw) = low <= n && n <= high
  where
    n = occurrences c pw

solve1 :: Input -> Int
solve1 = length . filter valid1

testInput :: String
testInput = "\
    \1-3 a: abcde\n\
    \1-3 b: cdefg\n\
    \2-9 c: ccccccccc\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 2)]

-- Part Two

valid2 :: Policy -> Bool
valid2 (Policy pos1 pos2 c pw) =
    occurrences c [pw!!(pos1 - 1), pw!!(pos2 - 1)] == 1

solve2 :: Input -> Int
solve2 = length . filter valid2

tests2 :: [(String, Int)]
tests2 = [(testInput, 1)]

main :: IO ()
main = do
    s <- readFile "input/02.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
