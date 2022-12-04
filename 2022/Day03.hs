module Main where

import Utilities
import Data.Char
import Data.Set (Set)
import qualified Data.Set as Set

-- Input processing

type Input = [Rucksack]
type Rucksack = [Item]
type Item = Char

parse :: String -> Input
parse = lines

-- Part One

halves :: [a] -> ([a], [a])
halves xs = splitAt (length xs `div` 2) xs

-- items in both compartments
common :: Rucksack -> Set Item
common xs = Set.intersection (Set.fromList c1) (Set.fromList c2)
  where
    (c1, c2) = halves xs

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

-- item in all the rucksacks
badge :: [Rucksack] -> Item
badge = head . Set.elems . foldr1 Set.intersection . map Set.fromList

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
