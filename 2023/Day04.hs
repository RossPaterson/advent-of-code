module Main where

import Parser
import Utilities
import Control.Applicative
import Data.List
import qualified Data.Set as Set

-- Input processing

type Input = [Card]
data Card = Card Int [Int] [Int]
    deriving (Show)

parse :: String -> Input
parse = map (runParser card) . lines
  where
    card =
        Card <$ string "Card" <* some space <*> nat <* char ':' <* spaces <*>
            some (nat <* spaces) <* char '|' <*> some (spaces *> nat)
    spaces = some space

-- Part One

-- count of numbers in the second group that are in the first
matches :: Card -> Int
matches (Card _ w ns) = length (filter (`Set.member` ws) ns)
  where
    ws = Set.fromList w

value :: Int -> Int
value n
  | n == 0 = 0
  | otherwise = 2^(n-1)

solve1 :: Input -> Int
solve1 = sum . map (value . matches)

testInput :: String
testInput = "\
    \Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\n\
    \Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\n\
    \Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\n\
    \Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\n\
    \Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\n\
    \Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 13)]

-- Part Two

-- n copies of a
data Repeated a = Repeat Int a
    deriving (Show)

-- one copy
single :: a -> Repeated a
single = Repeat 1

-- add k copies
add :: Int -> Repeated a -> Repeated a
add k (Repeat n x) = Repeat (k+n) x

-- each card c generates copies of the following (matches c) cards
play :: [Repeated Card] -> Maybe (Int, [Repeated Card])
play [] = Nothing
play (Repeat n c:cs) = Just (n, map (add n) f ++ b)
  where
    (f, b) = splitAt (matches c) cs

solve2 :: Input -> Int
solve2 = sum . unfoldr play . map single

tests2 :: [(String, Int)]
tests2 = [(testInput, 30)]

main :: IO ()
main = do
    s <- readFile "input/04.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
