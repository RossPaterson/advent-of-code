module Main where

import Utilities
import Parser
import Control.Applicative

type Name = String
data Ingredient = Ingredient {
    name :: String,
    capacity :: Int,
    durability :: Int,
    flavor :: Int,
    texture :: Int,
    calories :: Int }
  deriving Show
type Input = [Ingredient]

parse :: String -> Input
parse = map (runParser ingredient) . lines
  where
    ingredient =
        Ingredient <$> iname <*
            string ": capacity " <*> int <*
            string ", durability " <*> int <*
            string ", flavor " <*> int <*
            string ", texture " <*> int <*
            string ", calories " <*> int
    iname = some letter

type Recipe = [(Ingredient, Int)]

properties :: [Ingredient -> Int]
properties = [capacity, durability, flavor, texture]

total :: (Ingredient -> Int) -> Recipe -> Int
total f is = sum [f i*q | (i, q) <- is]

score :: Recipe -> Int
score is = product [max 0 (total f is) | f <- properties]

quantity :: Int
quantity = 100

selections :: Int -> [a] -> [[(a, Int)]]
selections n [] = [[] | n == 0]
selections n (x:xs) = [(x, m):ys | m <- [1..n], ys <- selections (n-m) xs]

testInput :: String
testInput =
    "Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8\n\
    \Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 62842880)]

solve1 :: Input -> Int
solve1 = maximum . map score . selections quantity

-- Part Two --

solve2 :: Input -> Int
solve2 = maximum . map score . filter ((== 500) . total calories) . selections quantity

tests2 :: [(String, Int)]
tests2 = [(testInput, 57600000)]

main :: IO ()
main = do
    s <- readFile "input/15.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
