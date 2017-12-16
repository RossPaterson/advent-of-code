module Main where

import Parser
import Utilities
import Control.Applicative
import Data.Array
import Data.Char
import Data.Monoid

data Move = Spin Int | Exchange Int Int | Partner Int Int
    deriving Show

type Input = [Move]

parse :: String -> Input
parse = map (runParser move) . words . map (\ c -> if c == ',' then ' ' else c)
  where
    move =
        Spin <$ char 's' <*> nat <|>
        Exchange <$ char 'x' <*> nat <* char '/' <*> nat <|>
        Partner <$ char 'p' <*> program <* char '/' <*> program
    program = letterToProgram <$> satisfy isLower

programLetter :: Int -> Char
programLetter n = chr (n + ord 'a')

letterToProgram :: Char -> Int
letterToProgram c = ord c - ord 'a'

-- mtimes k p = mconcat (replicate k) p, but with O(log k) operations
mtimes :: Monoid a => Int -> a -> a
mtimes k p
  | k == 0 = mempty
  | k == 1 = p
  | even k = pk2 <> pk2
  | otherwise = pk2 <> pk2 <> p
  where
    pk2 = mtimes (k `div` 2) p

-- Dances perform permutations of [0..n-1]

data Permutation = Identity | Permutation (Array Int Int)
    deriving Show

instance Monoid Permutation where
    mempty = Identity
    mappend = composePerm

composePerm :: Permutation -> Permutation -> Permutation
composePerm Identity p2 = p2
composePerm p1 Identity = p1
composePerm (Permutation p1) (Permutation p2) =
    Permutation (listArray (bounds p2) [p1!(p2!i) | i <- indices p2])

showPerm :: Permutation -> String
showPerm Identity = "<identity>"
showPerm (Permutation p) = map programLetter (elems p)

-- Basic permutations

spin :: Int -> Int -> Permutation
spin n i = Permutation (listArray (0, n-1) ([n-i..n-1] ++ [0..n-i-1]))

swap :: Int -> Int -> Int -> Permutation
swap n i j
  | i == j = Identity
  | otherwise = Permutation $
        listArray (0, n-1) ([0..p1-1] ++ p2:[p1+1..p2-1] ++ p1:[p2+1..n-1])
  where
    p1 = min i j
    p2 = max i j

{-
If a dance performs a permutation p, then adding one more move changes
the permutation as follows:

Spin i       : p <> spin i
Exchange i j : p <> swap n i j
Partner i j  : swap n i j <> p

Therefore we build a pair of permutations to be wrapped around the starting
permutation, with the front one composed in the reverse order.
-}

type Wrap a = (Dual a, a)

-- wrap the identity permutation
wrap :: Monoid a => Wrap a -> a
wrap (Dual pf, pb) = pf <> pb

movePermutations :: Int -> Move -> Wrap Permutation
movePermutations n (Spin i) = (mempty, spin n i)
movePermutations n (Exchange i j) = (mempty, swap n i j)
movePermutations n (Partner i j) = (Dual (swap n i j), mempty)

showDance :: Wrap Permutation -> String
showDance = showPerm . wrap

dance :: Int -> [Move] -> Wrap Permutation
dance n = mconcat . map (movePermutations n)

solve1 :: Input -> String
solve1 = showDance . dance 16

testInput :: String
testInput = "s1,x3/4,pe/b"

tests1 :: [((Int, [Move]), String)]
tests1 = [((5, parse testInput), "baedc")]

-- Part Two

solve2 :: Input -> String
solve2 = showDance . mtimes 1000000000 . dance 16

tests2 :: [((Int, [Move]), String)]
tests2 = [((5, parse testInput), "ceadb")]

main :: IO ()
main = do
    s <- readFile "input16.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (showDance . uncurry dance) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (showDance . mtimes 2 . uncurry dance) tests2))
    print (solve2 input)
