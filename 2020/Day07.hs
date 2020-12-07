module Main where

import Graph
import Utilities
import Parser
import Control.Applicative
import Data.Char
import Data.Map (Map, (!))
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Input processing

type Input = Contents Colour

-- for each item, a list of items and quantities it contains
type Contents a = Map a [(Int, a)]
type Colour = String

parse :: String -> Input
parse = Map.fromList . map (runParser rule) . lines
  where
    rule = (,) <$> colour <* string " bags contain " <*> rhs <* char '.'
    colour = (\ adv adj -> adv ++ " " ++ adj) <$> word <* space <*> word
    rhs =
        pure [] <* string "no other bags" <|>
        sepBy1 subbag (string ", ")
    subbag = (,) <$> nat <* char ' ' <*> colour <* string " bag" <* suffix
    suffix = pure ' ' <|> char 's'
    word = many (satisfy isLower)

-- Part One

-- inverse of a relation
invert :: (Ord a, Ord b) => Map a [b] -> Map b [a]
invert m = Map.map Set.elems $ Map.unionsWith Set.union
    [Map.singleton y (Set.singleton x) | (x, ys) <- Map.toList m, y <- ys]

-- transitive containers of x
containers :: Ord a => a -> Contents a -> [a]
containers x rs = concat $ tail $ bfs wrapper [x]
  where
    wrapper y = Map.findWithDefault [] y wrapper_map
    wrapper_map = invert (Map.map (map snd) rs)

solve1 :: Input -> Int
solve1 = length . containers "shiny gold"

testInput1 :: String
testInput1 = "\
    \light red bags contain 1 bright white bag, 2 muted yellow bags.\n\
    \dark orange bags contain 3 bright white bags, 4 muted yellow bags.\n\
    \bright white bags contain 1 shiny gold bag.\n\
    \muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.\n\
    \shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.\n\
    \dark olive bags contain 3 faded blue bags, 4 dotted black bags.\n\
    \vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.\n\
    \faded blue bags contain no other bags.\n\
    \dotted black bags contain no other bags.\n"

tests1 :: [(String, Int)]
tests1 = [(testInput1, 4)]

-- Part Two

-- map of total number of subitems of each item
-- (memoized, implementing dynamic programming, though not needed here)
subitems :: Ord a => Map a [(Int, a)] -> Map a Int
subitems m = count_map
  where
    count_map = Map.map (\ nis -> sum [n*(1+count_map!i) | (n, i) <- nis]) m

solve2 :: Input -> Int
solve2 = Map.findWithDefault 0 "shiny gold" . subitems

testInput2 :: String
testInput2 = "\
    \shiny gold bags contain 2 dark red bags.\n\
    \dark red bags contain 2 dark orange bags.\n\
    \dark orange bags contain 2 dark yellow bags.\n\
    \dark yellow bags contain 2 dark green bags.\n\
    \dark green bags contain 2 dark blue bags.\n\
    \dark blue bags contain 2 dark violet bags.\n\
    \dark violet bags contain no other bags.\n"

tests2 :: [(String, Int)]
tests2 = [(testInput1, 32), (testInput2, 126)]

main :: IO ()
main = do
    s <- readFile "input/07.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
