module Main where

import Data.Char
import Data.Set (Set)
import qualified Data.Set as Set

type Pos = (Int, Int)

origin :: Pos
origin = (0, 0)

solve1 :: String -> Int
solve1 = Set.size . Set.fromList . scanl move origin . filter (not . isSpace)

move :: Pos -> Char -> Pos
move (x, y) '^' = (x, y-1)
move (x, y) 'v' = (x, y+1)
move (x, y) '<' = (x-1, y)
move (x, y) '>' = (x+1, y)
move _ _ = error "bad move"

tests = [">", "^>v<", "^v^v^v^v^v"]

-- Part Two --

odds :: [a] -> [a]
odds [] = []
odds (x:xs) = evens xs

evens :: [a] -> [a]
evens [] = []
evens (x:xs) = x : odds xs

solve2 :: String -> Int
solve2 s = Set.size (Set.fromList santa `Set.union` Set.fromList robo)
  where
    cs = filter (not . isSpace) s
    santa = scanl move origin (evens cs)
    robo = scanl move origin (odds cs)

tests2 = ["^v", "^>v<", "^v^v^v^v^v"]

main :: IO ()
main = do
    s <- readFile "input/03.txt"
    print (solve1 s)
    print (solve2 s)
