module Main where

import Parser
import Utilities
import Control.Applicative
import qualified Data.Set as Set

-- Input processing

type Input = [Card]
data Card = Card Int [Int] [Int]
    deriving (Show)

parse :: String -> Input
parse = map (runParser card) . lines
  where
    card = Card <$ string "Card" <* some space <*> nat <* char ':' <*> nats <* string " |" <*> nats
    nats = some (some space *> nat)

-- Part One

winning :: Card -> [Int]
winning (Card _ w ns) = filter (`Set.member` ws) ns
  where
    ws = Set.fromList w

matches :: Card -> Int
matches = length . winning

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

-- (count, matches)
type RepeatedCard = (Int, Int)

-- initially one copy of the card
oneCard :: Card -> RepeatedCard
oneCard c = (1, matches c)

-- make k copies of the cards
copy :: Int -> RepeatedCard -> RepeatedCard
copy k (n, m) = (k+n, m)

-- each card generates copies of the following m cards
play :: [RepeatedCard] -> [Int]
play [] = []
play ((n, m):cs) = n : play (map (copy n) f ++ b)
  where
    (f, b) = splitAt m cs

solve2 :: Input -> Int
solve2 = sum . play . map oneCard

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
