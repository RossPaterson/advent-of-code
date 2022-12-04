module Main where

import Utilities
import Parser

-- Input processing

type Input = [(Range, Range)]
data Range = Range Int Int
    deriving (Eq, Show)

parse :: String -> Input
parse s = map (runParser sections) (lines s)
  where
    sections = (,) <$> range <* char ',' <*> range
    range = Range <$> nat <* char '-' <*> nat

-- Part One

contains :: Range -> Range -> Bool
contains (Range lo1 hi1) (Range lo2 hi2) = lo1 <= lo2 && hi2 <= hi1

redundant :: (Range, Range) -> Bool
redundant (r1, r2) = contains r1 r2 || contains r2 r1

solve1 :: Input -> Int
solve1 = length . filter redundant

testInput :: String
testInput = "\
    \2-4,6-8\n\
    \2-3,4-5\n\
    \5-7,7-9\n\
    \2-8,3-7\n\
    \6-6,4-6\n\
    \2-6,4-8\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 2)]

-- Part Two

overlap :: Range -> Range -> Bool
overlap (Range lo1 hi1) (Range lo2 hi2) = lo1 <= hi2 && lo2 <= hi1

solve2 :: Input -> Int
solve2 = length . filter (uncurry overlap)

tests2 :: [(String, Int)]
tests2 = [(testInput, 4)]

main :: IO ()
main = do
    s <- readFile "input/04.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
