module Main where

import Utilities
import Data.Char
import Data.Set (Set)
import qualified Data.Set as Set

-- Input processing

type Input = [Rucksack]
type Rucksack = (Compartment, Compartment)
type Compartment = [Item]
type Item = Char

parse :: String -> Input
parse = map halves . lines

halves :: [a] -> ([a], [a])
halves xs = splitAt (length xs `div` 2) xs

-- Part One

-- items in both compartments
common :: Rucksack -> Set Item
common (xs, ys) = Set.intersection (Set.fromList xs) (Set.fromList ys)

priority :: Item -> Int
priority c
  | isLower c = ord c - ord 'a' + 1
  | otherwise = ord c - ord 'A' + 27

solve1 :: Input -> Int
solve1 = sum . map priority . concat . map (Set.toList . common)

testInput :: String
testInput = "\
    \vJrwpWtwJgWrhcsFMMfFFhFp\n\
    \jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\n\
    \PmmdzqPrVvPwwTWBwg\n\
    \wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\n\
    \ttgJtRGJQctTZtZT\n\
    \CrZsJsPPZsGzwwsLwLmpwMDw\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 157)]

-- Part Two

contents :: Rucksack -> Set Item
contents (xs, ys) = Set.union (Set.fromList xs) (Set.fromList ys)

-- item in all the rucksacks
badge :: [Rucksack] -> Item
badge = head . Set.elems . foldr1 Set.intersection . map contents

solve2 :: Input -> Int
solve2 = sum . map (priority . badge) . takes 3

tests2 :: [(String, Int)]
tests2 = [(testInput, 70)]

main :: IO ()
main = do
    s <- readFile "input/03.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
