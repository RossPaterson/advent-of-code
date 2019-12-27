module Main where

import Utilities
import Parser
import Control.Applicative
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

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
guests = fastNub . map snd . Map.keys

happiness :: Input -> Person -> Person -> Int
happiness m a b =
    fromMaybe 0 (Map.lookup (a, b) m) + fromMaybe 0 (Map.lookup (b, a) m)

-- fix the first one, since rotations make no difference
happiest :: Input -> [Person] -> Int
happiest _ [] = 0
happiest m (p:rest) =
    maximum [sum (zipWith (happiness m) (p:ps) (ps++[p])) |
        ps <- permutations rest]

solve1 :: Input -> Int
solve1 m = happiest m (guests m)

testInput :: String
testInput =
    "Alice would gain 54 happiness units by sitting next to Bob.\n\
    \Alice would lose 79 happiness units by sitting next to Carol.\n\
    \Alice would lose 2 happiness units by sitting next to David.\n\
    \Bob would gain 83 happiness units by sitting next to Alice.\n\
    \Bob would lose 7 happiness units by sitting next to Carol.\n\
    \Bob would lose 63 happiness units by sitting next to David.\n\
    \Carol would lose 62 happiness units by sitting next to Alice.\n\
    \Carol would gain 60 happiness units by sitting next to Bob.\n\
    \Carol would gain 55 happiness units by sitting next to David.\n\
    \David would gain 46 happiness units by sitting next to Alice.\n\
    \David would lose 7 happiness units by sitting next to Bob.\n\
    \David would gain 41 happiness units by sitting next to Carol.\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 330)]

-- Part Two --

solve2 :: Input -> Int
solve2 m = happiest m ("":guests m)

main :: IO ()
main = do
    s <- readFile "input/13.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    print (solve2 input)
