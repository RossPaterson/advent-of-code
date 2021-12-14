module Main where

import Utilities
import Parser
import Data.Maybe
import Data.Map (Map, (!))
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

-- naive string expansion
-- After n steps, an initial string of length k grows to (k-1)*2^n + 1.
expand :: Map Pair Element -> Polymer -> Polymer
expand m es =
    concat [maybeToList mb_ins ++ [e] | (mb_ins, e) <- zip ins es]
  where
    ins = Nothing : [Map.lookup pair m | pair <- pairs es]

summarize :: [Int] -> Int
summarize ns = maximum ns - minimum ns

solve1 :: Input -> Int
solve1 (template, rules) =
    summarize $ map snd $ frequency $ times 10 (expand m) template
  where
    m = Map.fromList rules

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

-- Faster version (which would also work for part one, of course)
-- Instead of the whole string, it suffices to record the frequency of
-- each element.  Missing elements have an implicit count of zero.
type Bag a = Map a Int

-- number of each kind of element in between each pair after n iterations
type Expansion = Map Pair (Bag Element)

-- all the elements mentioned in the problem
elements :: Polymer -> [Rule] -> [Element]
elements es rs = fastNub (es ++ concat [[l, r, e] | ((l, r), e) <- rs])

-- initially no elements between each pair
initExpansion :: [Element] -> Expansion
initExpansion es = Map.fromList [((e1, e2), Map.empty) | e1 <- es, e2 <- es]

expand2 :: Map Pair Element -> Expansion -> Expansion
expand2 m ems = Map.mapWithKey expandPair ems
  where
    expandPair :: Pair -> Bag Element -> Bag Element
    expandPair (e1, e2) _ = case Map.lookup (e1, e2) m of
        Nothing -> Map.empty
        Just e ->
            Map.unionsWith (+) [Map.singleton e 1, ems!(e1, e), ems!(e, e2)]

-- add the numbers of elements inserted between each pair in n steps
fullExpansion :: Polymer -> Expansion -> Bag Element
fullExpansion es em =
    Map.unionsWith (+)
        (Map.fromList (frequency es) : [em!pair | pair <- pairs es])

solve2 :: Input -> Int
solve2 (template, rules) = summarize $ Map.elems $ fullExpansion template em
  where
    m = Map.fromList rules
    em = times 40 (expand2 m) (initExpansion (elements template rules))

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
