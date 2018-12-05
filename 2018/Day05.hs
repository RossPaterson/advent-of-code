module Main where

import Utilities
import Data.Char

-- Input processing

type Input = FreeGroup Char

type FreeGroup a = [Unit a]
data Unit a = Unit { polarity :: Bool, absValue :: a }
  deriving (Show, Eq)

inverse :: Unit a -> Unit a
inverse (Unit p t) = Unit (not p) t

parse :: String -> Input
parse = map unit . filter (/= '\n')
  where
    unit c = Unit (isLower c) (toLower c)

-- Part One

solve1 :: Input -> Int
solve1 = length . normalize

-- normalize by repeatedly removing adjacent cancelling units
normalize :: Eq a => FreeGroup a -> FreeGroup a
normalize = reverse . foldl reduce []
  where
    -- accumulation parameter is reverse of reduced prefix
    reduce [] y = [y]
    reduce (x:xs) y
      | x == inverse y = xs
      | otherwise = y:x:xs

tests1 :: [(String, Int)]
tests1 = [("aA", 0), ("abBA", 0), ("abAB", 4), ("aabAAB", 6), ("dabAcCaCBAcCcaDA", 10)]

-- Part Two

solve2 :: Input -> Int
solve2 = shortest

-- shortest length obtained by removing all of one type of unit and reducing
shortest :: Ord a => FreeGroup a -> Int
shortest us =
    minimum [length (normalize (filter ((/= t) . absValue) us)) |
        t <- fast_nub (map absValue us)]

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
