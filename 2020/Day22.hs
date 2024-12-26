module Main where

import Utilities
import Data.Foldable
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sequence (Seq, ViewL(..), (|>))
import qualified Data.Sequence as Seq

-- Input processing

type Input = Decks
type Decks = (Deck, Deck)
type Deck = Seq Int

parse :: String -> Input
parse s = case paragraphs s of
    [p1, p2] -> (numbers p1, numbers p2)
    _ -> error "bad input"
  where
    numbers = Seq.fromList . map read . tail . lines

-- Part One

data Player = P1 | P2
    deriving Show

-- on each round, either return a winner with their deck or the next state
playRound1 :: Decks -> Either (Player, Deck) Decks
playRound1 (xs, ys) = case Seq.viewl xs of
    EmptyL -> Left (P2, ys)
    x :< xs' -> case Seq.viewl ys of
        EmptyL -> Left (P1, xs)
        y :< ys'
          | x > y -> Right (xs' |> x |> y, ys')
          | x < y -> Right (xs', ys' |> y |> x)
          | otherwise -> error "same card"

-- play game until someone wins
playGame1 :: Decks -> (Player, Deck)
playGame1 = whileRight playRound1

scoreDeck :: Deck -> Int
scoreDeck = sum . zipWith (*) [1..] . reverse . toList

solve1 :: Input -> Int
solve1 = scoreDeck . snd . playGame1

testInput :: String
testInput = "\
    \Player 1:\n\
    \9\n\
    \2\n\
    \6\n\
    \3\n\
    \1\n\
    \\n\
    \Player 2:\n\
    \5\n\
    \8\n\
    \4\n\
    \7\n\
    \10\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 306)]

-- Part Two

-- Since the set of cards does not change within a single game, we can
-- detect repetitions by checking one player's deck.  We do this for P1.
data State = State { seen :: Set Deck, decks :: Decks }
    deriving Show

-- on each round, either return a winner with their deck or the next state
playRound2 :: State -> Either (Player, Deck) State
playRound2 (State history (xs, ys))
  | Set.member xs history = Left (P1, xs)
  | otherwise = case Seq.viewl xs of
    EmptyL -> Left (P2, ys)
    x :< xs' -> case Seq.viewl ys of
        EmptyL -> Left (P1, xs)
        y :< ys' -> Right (State (Set.insert xs history) new_ds)
          where
            winner
              | x <= Seq.length xs' && y <= Seq.length ys' =
                fst (playGame2 (Seq.take x xs', Seq.take y ys'))
              | x > y = P1
              | x < y = P2
              | otherwise = error "same card"
            new_ds = case winner of
                P1 -> (xs' |> x |> y, ys')
                P2 -> (xs', ys' |> y |> x)

-- play game until someone wins
playGame2 :: Decks -> (Player, Deck)
playGame2 = whileRight playRound2 . State Set.empty

solve2 :: Input -> Int
solve2 = scoreDeck . snd . playGame2

tests2 :: [(String, Int)]
tests2 = [(testInput, 291)]

main :: IO ()
main = do
    s <- readFile "input/22.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
