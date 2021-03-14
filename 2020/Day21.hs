module Main where

import Utilities
import Matching
import Parser
import Control.Applicative
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
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

-- map each allergen to possible ingredients, i.e. those present in
-- each rule involving that allergen
possible :: (Ord a, Ord b) => [Rule a b] -> Map b (Set a)
possible rs =
    Map.fromListWith Set.intersection
        [(b, as) | Contains as bs <- rs, b <- Set.elems bs]

-- ingredients that could match some allergen
allPossible :: (Ord a, Ord b) => [Rule a b] -> Set a
allPossible rs = Set.unions (Map.elems (possible rs))

-- total number of unmatched ingredients in the rules, including repetitions
solve1 :: Input -> Int
solve1 rs = sum [Set.size (Set.difference as poss) | Contains as _ <- rs]
  where
    poss = allPossible rs

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
solve2 = intercalate "," . Map.elems .  uniquePerfectMatching . possible

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
