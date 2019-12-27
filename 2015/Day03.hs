module Main where

import Utilities
import Data.Char
import qualified Data.Set as Set

type Pos = (Int, Int)

origin :: Pos
origin = (0, 0)

parse :: String -> String
parse = filter (not . isSpace)

solve1 :: String -> Int
solve1 = Set.size . Set.fromList . scanl move origin

move :: Pos -> Char -> Pos
move (x, y) '^' = (x, y-1)
move (x, y) 'v' = (x, y+1)
move (x, y) '<' = (x-1, y)
move (x, y) '>' = (x+1, y)
move _ _ = error "bad move"

tests1 :: [(String, Int)]
tests1 = [
    (">", 2),
    ("^>v<", 4),
    ("^v^v^v^v^v", 2)]

-- Part Two --

odds :: [a] -> [a]
odds [] = []
odds (_:xs) = evens xs

evens :: [a] -> [a]
evens [] = []
evens (x:xs) = x : odds xs

solve2 :: String -> Int
solve2 s = Set.size (Set.fromList santa `Set.union` Set.fromList robo)
  where
    cs = filter (not . isSpace) s
    santa = scanl move origin (evens cs)
    robo = scanl move origin (odds cs)

tests2 :: [(String, Int)]
tests2 = [
    ("^v", 3),
    ("^>v<", 3),
    ("^v^v^v^v^v", 11)]

main :: IO ()
main = do
    s <- readFile "input/03.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
