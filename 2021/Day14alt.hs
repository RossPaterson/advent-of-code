-- alternative solution counting pairs of adjacent elements
module Main where

import Utilities
import Parser
import Data.Map (Map)
import qualified Data.Map as Map

-- Input processing

type Element = Char
type Polymer = [Element]
type Pair = (Element, Element)
type Rule = (Pair, Element)
type Input = (Polymer, [Rule])

parse :: String -> Input
parse s = (head (lines template), map (runParser rule) (lines rules))
  where
    [template, rules] = paragraphs s
    rule = (,) <$> pair <* string " -> " <*> element
    pair = (,) <$> element <*> element
    element = letter

-- Part One

-- pairs of adjacent elements
pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

-- we will record the number of occurrences of each pair after n iterations
type Bag a = Map a Int

-- number of occurrences of each element of the list
counts :: Ord a => [a] -> Bag a
counts xs = Map.fromListWith (+) [(x, 1) | x <- xs]

-- Expand a bag of pairs.  This is a linear transformation, so it could
-- be expressed as a large matrix and efficiently raised to a power,
-- but that is not needed for only 40 iterations.
expand :: Map Pair Element -> Bag Pair -> Bag Pair
expand m ps =
    Map.fromListWith (+) [(p, n) |
        ((e1, e2), n) <- Map.assocs ps,
        p <- case Map.lookup (e1, e2) m of
            Nothing -> [(e1, e2)]
            Just e -> [(e1, e), (e, e2)]]

-- Get element counts from pair counts.
-- Each element in the string is counted twice except the first and last
-- elements of the original string, so we add those before halving.
elementCounts :: Polymer -> Bag Pair -> Bag Element
elementCounts es ps =
    fmap (`div` 2) $
    Map.fromListWith (+) $
        (head es, 1) :
        (last es, 1) :
        [(e, n) | ((e1, e2), n) <- Map.assocs ps, e <- [e1, e2]]

summarize :: [Int] -> Int
summarize ns = maximum ns - minimum ns

solve :: Int -> Input -> Int
solve n (template, rules) =
    summarize $ Map.elems $
        elementCounts template $ times n (expand m) $ counts $ pairs template
  where
    m = Map.fromList rules

solve1 :: Input -> Int
solve1 = solve 10

testInput :: String
testInput = "\
    \NNCB\n\
    \\n\
    \CH -> B\n\
    \HH -> N\n\
    \CB -> H\n\
    \NH -> C\n\
    \HB -> C\n\
    \HC -> B\n\
    \HN -> C\n\
    \NN -> C\n\
    \BH -> H\n\
    \NC -> B\n\
    \NB -> B\n\
    \BN -> B\n\
    \BB -> N\n\
    \BC -> B\n\
    \CC -> N\n\
    \CN -> C\n\
    \"

tests1 :: [(String, Int)]
tests1 = [(testInput, 1588)]

-- Part Two

solve2 :: Input -> Int
solve2 = solve 40

tests2 :: [(String, Int)]
tests2 = [(testInput, 2188189693529)]

main :: IO ()
main = do
    s <- readFile "input/14.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
