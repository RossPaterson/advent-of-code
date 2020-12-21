module Main where

import Utilities
import Parser
import Control.Applicative
import Data.List
import Data.Maybe
import Data.Ord
import Data.Set (Set)
import qualified Data.Set as Set

-- Input processing

type Input = [Rule Ingredient Allergen]

data Rule a b = Contains (Set a) (Set b)
    deriving (Eq, Ord, Show)
type Ingredient = String
type Allergen = String

parse :: String -> Input
parse = map (runParser rule) . lines
  where
    rule = Contains <$> ingredients <*
        string " (contains " <*> allergens <* char ')'
    ingredients = Set.fromList <$> sepBy1 word (char ' ')
    allergens = Set.fromList <$> sepBy1 word (string ", ")
    word = some letter

-- Part One

{-
Aim: given a list of rules relating P A and P B, find a one-to-one map

    inj :: B -> A

such that for each rule Contains as bs, map inj bs is included in as.
-}

-- non-redundant intersection closure of a list of rules
closeRules :: (Ord a, Ord b) => [Rule a b] -> [Rule a b]
closeRules = foldr addRule []

-- Add a rule and all its intersections to a non-redundant list of rules,
-- pruning redundant rules from the result.
addRule :: (Ord a, Ord b) => Rule a b -> [Rule a b] -> [Rule a b]
addRule r rs = foldr addMinRule rs (r:mapMaybe (intersectRule r) rs)

-- Add a rule to a non-redundant list, pruning any new redundancy.
addMinRule :: (Ord a, Ord b) => Rule a b -> [Rule a b] -> [Rule a b]
addMinRule r rs
  | any (flip implies r) rs = rs
  | otherwise = r:filter (not . implies r) rs

-- the first rule implies the second
implies :: (Ord a, Ord b) => Rule a b -> Rule a b -> Bool
implies (Contains as1 bs1) (Contains as2 bs2) =
    bs1 == bs2 && Set.isSubsetOf as1 as2

-- intersection of two rules, if nontrivial
intersectRule :: (Ord a, Ord b) => Rule a b -> Rule a b -> Maybe (Rule a b)
intersectRule (Contains as1 bs1) (Contains as2 bs2)
  | Set.disjoint bs1 bs2 = Nothing
  | otherwise = Just $
    Contains (Set.intersection as1 as2) (Set.intersection bs1 bs2)

-- the rule implies a matching
simple :: (Ord a, Ord b) => Rule a b -> Maybe (a, b)
simple (Contains as bs)
  | Set.size as == 1 && Set.size bs == 1 =
    Just (Set.findMin as, Set.findMin bs)
  | otherwise = Nothing

-- one-to-one matching between two sets
type Iso a b = [(a, b)]

data State a b = State {
    rules :: [Rule a b], -- non-redundant closed collection of rules
    matches :: Iso a b
    }
    deriving Show

-- Start with the non-redundant closure of the given rules, and no matches.
initState :: (Ord a, Ord b) => [Rule a b] -> State a b
initState rs = State (closeRules rs) []

-- We are finished when all the rules have been turned into matches.
finished :: State a b -> Bool
finished s = null (rules s)

-- One deduction step:
-- * turn one-to-one rules into matchings
-- * remove the matched elements from all rules
-- * delete trivial rules
-- * delete redundant rules
reduceState :: (Ord a, Ord b) => State a b -> State a b
reduceState s = State {
    rules = new_rules,
    matches = new_matches ++ matches s
    }
  where
    new_matches = mapMaybe simple (rules s)
    matched_as = Set.fromList (map fst new_matches)
    matched_bs = Set.fromList (map snd new_matches)
    removeMatches (Contains as bs) =
        Contains (Set.difference as matched_as) (Set.difference bs matched_bs)
    new_rules =
        foldr addMinRule [] $
        filter (not . trivialRule) $
        map removeMatches $
        filter (null . simple) $
        rules s

-- A rule with no allergens tells us nothing.
trivialRule :: Rule a b -> Bool
trivialRule (Contains _ bs) = Set.null bs

-- Deduce the matching implied by a list of rules.
deduceMatching :: (Ord a, Ord b) => [Rule a b] -> Iso a b
deduceMatching = matches . until finished reduceState . initState

-- total number of unmatched ingredients in the rules, including repetitions
solve1 :: Input -> Int
solve1 rs = sum [Set.size (Set.difference as bad) | Contains as _ <- rs]
  where
    bad = Set.fromList (map fst (deduceMatching rs))

testInput :: String
testInput = "\
    \mxmxvkd kfcds sqjhc nhms (contains dairy, fish)\n\
    \trh fvjkl sbzzf mxmxvkd (contains dairy)\n\
    \sqjhc fvjkl (contains soy)\n\
    \sqjhc mxmxvkd sbzzf (contains fish)\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 5)]

-- Part Two

-- list of matched ingredients
solve2 :: Input -> String
solve2 = intercalate "," . map fst . sortBy (comparing snd) . deduceMatching

tests2 :: [(String, String)]
tests2 = [(testInput, "mxmxvkd,sqjhc,fvjkl")]

main :: IO ()
main = do
    s <- readFile "input/21.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    putStrLn (solve2 input)
