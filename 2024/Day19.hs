module Main where

import Parser
import Utilities
import Control.Applicative
import Data.List
import Data.Map ((!))
import qualified Data.Map as Map

-- Input processing

type Input = ([Pattern], [Design])

type Pattern = String
type Design = String

parse :: String -> Input
parse s = case paragraphs s of
    [p1, p2] -> (runParser patterns p1, lines p2)
    _ -> error "bad input"
  where
    patterns = some letter `sepBy1` string ", " <* char '\n'

-- Part One

solve1 :: Input -> Int
solve1 (patterns, designs) =
    length (filter (possible patterns) designs)

-- is it possible to assemble xs as a concatenation of elements of parts?
possible :: Eq a => [[a]] -> [a] -> Bool
possible parts xs = ways parts xs > 0

-- ways of assembling xs as a concatenation of elements of parts:
-- memoization of ways_slow
ways :: Eq a => [[a]] -> [a] -> Int
ways parts xs = ways_from 0
  where
    -- ways to assemble drop i xs
    ways_from i = ways_from_map!i
    -- memo table for ways_from
    ways_from_map = Map.mapWithKey ways_from_aux tail_map
    -- ways to assemble from_i = drop i xs
    ways_from_aux i from_i
      | null from_i = 1
      | otherwise =
        sum [ways_from (i + length p) | p <- parts, p `isPrefixOf` from_i]
    -- maps i to drop i xs
    tail_map = Map.fromList (zip [0..] (tails xs))

-- ways of assembling xs as a concatenation of elements of parts:
-- very slow direct implementation
ways_slow :: Eq a => [[a]] -> [a] -> Int
ways_slow parts xs
  | null xs = 1
  | otherwise =
    sum [ways_slow parts (drop (length p) xs) | p <- parts, p `isPrefixOf` xs]

testInput :: String
testInput = "\
    \r, wr, b, g, bwu, rb, gb, br\n\
    \\n\
    \brwrr\n\
    \bggr\n\
    \gbbr\n\
    \rrbgbr\n\
    \ubwu\n\
    \bwurrg\n\
    \brgr\n\
    \bbrgwb\n\
    \"

tests1 :: [(String, Int)]
tests1 = [(testInput, 6)]

-- Part Two

solve2 :: Input -> Int
solve2 (patterns, designs) = sum (map (ways patterns) designs)

tests2 :: [(String, Int)]
tests2 = [(testInput, 16)]

main :: IO ()
main = do
    s <- readFile "input/19.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
