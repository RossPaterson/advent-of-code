module Main where

import Parser
import Utilities
import Control.Applicative
import Data.Char
import Data.List
import Data.Ord

-- Input processing

type Input = [Hand]
data Hand = Hand [Card] Int
    deriving (Show)
data Card = N Int | T | J | Q | K | A
    deriving (Eq, Ord, Show)

parse :: String -> Input
parse = map (runParser hand) . lines
  where
    hand = Hand <$> some card <* space <*> nat
    card = face <|> (N . digitToInt) <$> digit
    face =
        T <$ char 'T' <|> J <$ char 'J' <|> Q <$ char 'Q' <|>
        K <$ char 'K' <|> A <$ char 'A'

-- Part One

-- hand type as in standard Poker
data HandType
    = HighCard
    | OnePair
    | TwoPair
    | ThreeOfAKind
    | FullHouse
    | FourOfAKind
    | FiveOfAKind
    deriving (Eq, Ord, Show)

-- determine type of hand as in standard Poker
handType :: [Card] -> HandType
handType cs = case sortBy (comparing Down) (map snd (frequency cs)) of
    [5] -> FiveOfAKind
    [4, 1] -> FourOfAKind
    [3, 2] -> FullHouse
    [3, 1, 1] -> ThreeOfAKind
    [2, 2, 1] -> TwoPair
    [2, 1, 1, 1] -> OnePair
    [1, 1, 1, 1, 1] -> HighCard
    _ -> error "unclassifiable hand"

-- break ties of same hand type by lexicographical ordering of the cards
-- (NOT the standard Poker rule)
value1 :: [Card] -> (HandType, [Card])
value1 cs = (handType cs, cs)

-- rank hands according to their value and multiply ranks by bids
winnings :: (Ord a) => ([Card] -> a) -> [Hand] -> Int
winnings value hs =
    sum $ zipWith (*) [1..] $ map snd $ sort [(value cs, n) | Hand cs n <- hs]

solve1 :: Input -> Int
solve1 = winnings value1

testInput :: String
testInput = "\
    \32T3K 765\n\
    \T55J5 684\n\
    \KK677 28\n\
    \KTJJT 220\n\
    \QQQJA 483\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 6440)]

-- Part Two

-- Joker rule: best hand type, with ties resolved by devaluing jokers
joker :: [Card] -> (HandType, [Card])
joker cs = (bestType cs, map devalue cs)

-- best hand type obtainable by replacing jokers with other cards
bestType :: [Card] -> HandType
bestType cs = maximum (map handType (traverse wildcard cs))

-- replace J cards with any other card
wildcard :: Card -> [Card]
wildcard J = [A, K, Q, T] ++ map N [2..9]
wildcard c = [c]

-- value J cards below all others
devalue :: Card -> Card
devalue J = N 1
devalue c = c

solve2 :: Input -> Int
solve2 = winnings joker

tests2 :: [(String, Int)]
tests2 = [(testInput, 5905)]

main :: IO ()
main = do
    s <- readFile "input/07.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
