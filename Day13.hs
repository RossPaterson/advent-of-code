module Main where

import Parser
import Control.Applicative
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Person = String
type Input = Map (Person, Person) Int

parse :: String -> Input
parse = Map.fromList . map (runParser statement) . lines
  where
    statement =
        assoc <$> person <* string " would " <*> gain <*
            string " happiness units by sitting next to " <*> person <* char '.'
    person = some letter
    gain = string "gain " *> nat <|> negate <$ string "lose " <*> nat
    assoc p1 g p2 = ((p1, p2), g)

guests :: Input -> [Person]
guests = Set.toList . Set.fromList . map snd . Map.keys

happiness :: Input -> Person -> Person -> Int
happiness m a b =
    fromMaybe 0 (Map.lookup (a, b) m) + fromMaybe 0 (Map.lookup (b, a) m)

total_happiness :: Input -> [Person] -> Int
total_happiness m [] = 0
total_happiness m (p:ps) = sum (zipWith (happiness m) (p:ps) (ps++[p]))

solve1 :: Input -> Int
solve1 m = maximum [total_happiness m ps | ps <- permutations (guests m)]

-- Part Two --

solve2 :: Input -> Int
solve2 m = maximum [total_happiness m ps | ps <- permutations ("":guests m)]

main :: IO ()
main = do
    s <- readFile "input13.txt"
    let input = parse s
    print (solve1 input)
    print (solve2 input)
