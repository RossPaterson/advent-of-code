module Main where

import Parser
import Utilities
import Data.List
import qualified Data.Set as Set

-- Input processing

type Input = ([Rule Int], [[Int]])

-- rule stating the first thing should come before the second
data Rule a = Before a a
    deriving (Eq, Ord, Show)

parse :: String -> Input
parse s = case paragraphs s of
    [p1, p2] ->
        (map (runParser rule) (lines p1), map (runParser list) (lines p2))
    _ -> error "bad input"
  where
    rule = Before <$> int <* char '|' <*> int
    list = int `sepBy1` char ','

-- Part One

solve1 :: Input -> Int
solve1 (rs, ns) = sum $ map middle $ filter (inOrder rs) ns

inOrder :: Eq a => [Rule a] -> [a] -> Bool
inOrder rs xs = all (conforms xs) rs

-- A list conforms to the rule if it does not contain a hi before a lo.
-- (assuming lo /= hi, which is true for our inputs)
conforms :: Eq a => [a] -> Rule a -> Bool
conforms xs (Before lo hi) = not (elem lo (dropWhile (/= hi) xs))

-- middle element of a list, assumed to be of odd length
middle :: [a] -> a
middle xs = xs !! (length xs `div` 2)

testInput :: String
testInput = "\
    \47|53\n\
    \97|13\n\
    \97|61\n\
    \97|47\n\
    \75|29\n\
    \61|13\n\
    \75|53\n\
    \29|13\n\
    \97|29\n\
    \53|29\n\
    \61|53\n\
    \97|53\n\
    \61|29\n\
    \47|13\n\
    \75|47\n\
    \97|75\n\
    \47|61\n\
    \75|61\n\
    \47|29\n\
    \75|13\n\
    \53|13\n\
    \\n\
    \75,47,61,53,29\n\
    \97,61,53,29,13\n\
    \75,29,13\n\
    \75,97,47,61,53\n\
    \61,13,29\n\
    \97,13,75,29,47\n\
    \"

tests1 :: [(String, Int)]
tests1 = [(testInput, 143)]

-- Part Two

{-
The test input and puzzle input are similar in that both define directed
graphs with exactly one edge between any two distinct nodes that occur
in the update lists.  They differ in that the test input defines a linear
ordering, while the puzzle input is a cyclic graph.  However, the subgraph
restricted to the nodes in each update list is always acyclic and defines
a linear ordering on those nodes.
-}

solve2 :: Input -> Int
solve2 (rs, ns) =
    sum $
    map (middle . sortBy (compareWith rs)) $
    filter (not . inOrder rs) ns

-- Compare two values using the rules, which consist of all the less-than
-- relationships between the values used.
compareWith :: Ord a => [Rule a] -> a -> a -> Ordering
compareWith rs = cmp
  where
    rel = Set.fromList rs
    cmp x y
      | Set.member (Before x y) rel = LT
      | x == y = EQ
      | otherwise = GT

tests2 :: [(String, Int)]
tests2 = [(testInput, 123)]

main :: IO ()
main = do
    s <- readFile "input/05.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
