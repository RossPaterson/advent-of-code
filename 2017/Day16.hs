module Main where

import Parser
import Data.SmallPermutation
import Utilities
import Control.Applicative
import Data.Char
import Data.Monoid
import Data.Semigroup

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

-- Dances perform permutations of [0..n-1]

showPerm :: Int -> SmallPermutation Int -> String
showPerm n p = map (programLetter . apply p) [0..n-1]

-- Basic permutation

spin :: Int -> Int -> SmallPermutation Int
spin n i = swapRanges 0 (n-i) n

{-
If a dance performs a permutation p, then adding one more move changes
the permutation as follows:

Spin i       : p <> spin i
Exchange i j : p <> swap i j
Partner i j  : swap i j <> p

Therefore we build a pair of permutations to be wrapped around the starting
permutation, with the front one composed in the reverse order.
-}

type Wrap a = (Dual a, a)

-- wrap the identity permutation
wrap :: Monoid a => Wrap a -> a
wrap (Dual pf, pb) = pf <> pb

type Dance = Wrap (SmallPermutation Int)

danceMove :: Int -> Move -> Dance
danceMove n (Spin i) = (mempty, spin n i)
danceMove _ (Exchange i j) = (mempty, swap i j)
danceMove _ (Partner i j) = (Dual (swap i j), mempty)

showDance :: Int -> Dance -> String
showDance n = showPerm n . wrap

dance :: Int -> [Move] -> Dance
dance n = mconcat . map (danceMove n)

solve1 :: Input -> String
solve1 = showDance 16 . dance 16

testInput :: String
testInput = "s1,x3/4,pe/b"

tests1 :: [((Int, Int, [Move]), String)]
tests1 = [((5, 1, parse testInput), "baedc")]

runTest :: (Int, Int, [Move]) -> String
runTest (size, rep, ms) = showDance size (stimesMonoid rep (dance size ms))

-- Part Two

solve2 :: Input -> String
solve2 = showDance 16 . stimesMonoid (1000000000::Int) . dance 16

tests2 :: [((Int, Int, [Move]), String)]
tests2 = [((5, 2, parse testInput), "ceadb")]

main :: IO ()
main = do
    s <- readFile "input/16.txt"
    let input = parse s
    putStr (unlines (failures "solve1" runTest tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" runTest tests2))
    print (solve2 input)
