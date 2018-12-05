module Main where

import Utilities
import Data.Char

-- Input processing

type Input = [Unit Char]

data Unit a = Unit { polarity :: Bool, uType :: a }
  deriving Show

parse :: String -> Input
parse = map unit . filter (/= '\n')
  where
    unit c = Unit (isLower c) (toLower c)

-- Part One

solve1 :: Input -> Int
solve1 = length . reduce

-- repeatedly remove adjacent cancelling units
reduce :: Eq a => [Unit a] -> [Unit a]
reduce = reverse . react []
  where
    react xs [] = xs
    react [] (y:ys) = react [y] ys
    react (x:xs) (y:ys)
      | cancel x y = react xs ys
      | otherwise = react (y:x:xs) ys

-- two units cancel if they have the same type and opposite polarities
cancel :: Eq a => Unit a -> Unit a -> Bool
cancel (Unit p1 t1) (Unit p2 t2) = p1 == not p2 && t1 == t2

tests1 :: [(String, Int)]
tests1 = [("aA", 0), ("abBA", 0), ("abAB", 4), ("aabAAB", 6), ("dabAcCaCBAcCcaDA", 10)]

-- Part Two

solve2 :: Input -> Int
solve2 = shortest

-- shortest length obtained by removing all of one type of unit and reducing
shortest :: Ord a => [Unit a] -> Int
shortest us =
    minimum [length (reduce (filter ((/= t) . uType) us)) |
        t <- fast_nub (map uType us)]

tests2 :: [(String, Int)]
tests2 = [("dabAcCaCBAcCcaDA", 4)]

main :: IO ()
main = do
    s <- readFile "input05.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
